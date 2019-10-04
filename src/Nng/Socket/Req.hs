module Nng.Socket.Req
  ( ReqSocket
  , open
  , send
  ) where

import Nng.Error
import Nng.Prelude
import Nng.Socket.Internal
import qualified Libnng


data ReqSocket
  = ReqSocket
  { reqSocketSocket :: Libnng.Socket
  , reqSocketThread :: ThreadId
  , reqSocketVarsVar :: TMVar Vars
  }

data Vars
  = Vars
      -- Request var: client writes to this TMVar (usually only once), and this
      -- socket reads from it.
      --
      -- The client may put to the TMVar again, which means "ignore my previous
      -- one, send this one instead", and the reply to the original request (if
      -- received) will be tossed (by nng).
      ( TMVar ByteString )
      -- "Request accepted" var: clients read from this TMVar after writing a
      -- *new* request, and we write to it if we accept their new request to
      -- send.
      --
      -- This is necessary because the client needs to know which of its
      -- requests it eventually receives a response to. Without this
      -- back-and-forth between client and this socket, the following might
      -- occur:
      --
      --   1. Client puts request #1 to request TMVar
      --   2. Socket sends it and receives response #1
      --   3. Client times out waiting on response TMVar, and puts request #2
      --      to request TMVar
      --   4. Socket puts response #1 to response TMVar
      --   5. Client reads response #1 from response TMVar, even though it
      --      (seemigly) already enqueued request #2 instead.
      ( TMVar () )
      -- Response var: client reads from this TMVar, this socket writes to it.
      ( TMVar ( Either Error ByteString ) )

instance IsSocket ReqSocket where
  close = closeImpl

  openDialer    = reqSocketSocket >>> internalOpenDialer
  openDialer_   = reqSocketSocket >>> internalOpenDialer_
  openListener  = reqSocketSocket >>> internalOpenListener
  openListener_ = reqSocketSocket >>> internalOpenListener_

closeImpl
  :: ReqSocket
  -> IO ( Either Error () )
closeImpl socket = do
  killThread ( reqSocketThread socket )
  errnoToError ( Libnng.close ( reqSocketSocket socket ) )


--------------------------------------------------------------------------------
-- Open
--------------------------------------------------------------------------------

open :: IO ( Either Error ReqSocket )
open =
  Libnng.req0_open >>= \case
    Left err ->
      pure ( Left ( cintToError err ) )

    Right socket -> do
      varsVar :: TMVar Vars <-
        newEmptyTMVarIO

      managerThreadId :: ThreadId <-
        forkIO ( unsafeUnmask ( managerThread socket varsVar ) )

      pure
        ( Right ReqSocket
            { reqSocketSocket = socket
            , reqSocketThread = managerThreadId
            , reqSocketVarsVar = varsVar
            }
        )

managerThread
  :: Libnng.Socket
  -> TMVar Vars
  -> IO ()
managerThread socket varsVar =
  forever do
    Vars requestVar requestAcceptedVar responseVar <-
      atomically ( takeTMVar varsVar )

    response :: Either Error ByteString <-
      handleRequest
        socket
        ( takeTMVar requestVar )
        ( atomically ( putTMVar requestAcceptedVar () ) )

    atomically ( putTMVar responseVar response )

-- [Send request]
--   Error            => [Return Error]
--   Success          => [Wait for response]
--
-- [Wait for response]
--   Error            => [Return Error]
--   Success          => [Return Success]
--   Socket not ready => [Standby]
--
-- [Standby]
--   New request      => [Send request]
--   Socket ready     => [Wait for response]
handleRequest
  :: Libnng.Socket
  -> STM ByteString
  -> IO ()
  -> IO ( Either Error ByteString )
handleRequest socket takeRequest requestAccepted =
  atomically takeRequest >>= exchange

  where
    exchange
      :: ByteString
      -> IO ( Either Error ByteString )
    exchange bytes =
      internalSendByteString socket bytes >>= \case
        Left err ->
          pure ( Left err )

        Right () ->
          attemptRecv ( internalRecvByteString socket )

    attemptRecv
      :: IO ( Fix RecvF )
      -> IO ( Either Error ByteString )
    attemptRecv action =
      fmap unFix action >>= \case
        RecvF'Error err ->
          pure ( Left err )

        RecvF'Ok response ->
          pure ( Right response )

        RecvF'Again ready uninterested again ->
          let
            action1 :: STM ( IO ( Either Error ByteString ) )
            action1 = do
              ready
              pure ( attemptRecv again )

            action2 :: STM ( IO ( Either Error ByteString ) )
            action2 = do
              newRequest :: ByteString <-
                takeRequest

              pure do
                uninterested
                requestAccepted
                exchange newRequest

          in
            join ( atomically ( action1 <|> action2 ) )


--------------------------------------------------------------------------------
-- Send
--------------------------------------------------------------------------------

-- | Send a request on a @req@ socket and receive its reply.
--
-- Sending occurs in two steps, both of which are @STM@ actions that can be
-- canceled (as by e.g. 'Control.Concurrent.STM.TVar.registerDelay').
--
-- The first step is /send/, which blocks if the socket is currently being used
-- by another thread.
--
-- When it succeeds, it returns an /update/ action and a /receive/ action.
--
-- The /receive/ action blocks until a response to the latest request sent is
-- received from a @rep@ socket.
--
-- The /update/ action attempts to discard the latest request and send a new
-- one. However, a response to an outstanding request may arrive concurrently.
-- For this reason, the update action returns an @STM@ action that, if
-- successful, indicates the socket will send the new request, and any responses
-- to old requests will be discarded (by @nng@).
--
-- This @STM@ action should typically be raced against the receive action,
-- because if the receive action returns, then the update action alone will
-- throw 'Control.Exception.BlockedIndefinitelyOnSTM' (which is to say, if a
-- full request-response cycle has completed, the socket will /not/ accept an
-- update to an outstanding request, because there is none).
--
-- A common way to use 'send' may be to perform the full exchange to completion,
-- not updating the initial request after it's sent, and not bothering to time
-- out or utilize @STM@ in other ways during either the send or receive steps.
-- You may wish to define a helper function for this simple alternative:
--
-- @
-- simpleSend
--   :: 'ReqSocket'
--   -> ByteString
--   -> IO ( Either 'Error' ByteString )
-- simpleSend socket request = do
--   await1 <- 'send' socket request
--   ( _update, await2 ) <- atomically await1
--   atomically await2
-- @
--
-- Below is an example of using the update action properly. We send one request,
-- sleep for 5 seconds, then attempt update the request, which would succeed
-- only if we hadn't already received a response.
--
-- @
-- await1 <- 'send' socket request1
-- ( update, await2 ) <- atomically await1
--
-- threadDelay 5000000
--
-- success <- 'update' request2
-- atomically ( Left <$> success <|> Right <$> await2 ) >>= \\case
--   -- Request was updated, now await its response
--   Left () ->
--     atomically await2
--
--   -- Already received a response to the first request
--   Right result ->
--     pure result
-- @
send
  :: ReqSocket
  -> ByteString
  -> IO
       ( STM
           ( ByteString -> IO ( STM () )
           , STM ( Either Error ByteString )
           )
       )
send socket request = do
  requestVar :: TMVar ByteString <-
    newTMVarIO request

  requestAcceptedVar <-
    newEmptyTMVarIO

  responseVar :: TMVar ( Either Error ByteString ) <-
    newEmptyTMVarIO

  pure do
    putTMVar
      ( reqSocketVarsVar socket )
      ( Vars requestVar requestAcceptedVar responseVar )
    pure
      ( \newRequest -> do
          -- First, overwrite our old request.
          atomically do
            void ( tryTakeTMVar requestVar )
            putTMVar requestVar newRequest

          -- But block until we're sure the socket has actually "accepted" our
          -- new request to send.
          pure ( takeTMVar requestAcceptedVar )
      , takeTMVar responseVar
      )
