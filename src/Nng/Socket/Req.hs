module Nng.Socket.Req
  ( ReqSocket
  , open
  , send
  , Request
  , makeRequest
  , updateRequest
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

data Request
  = Request
      ( TMVar ByteString )
      ( TMVar () )

makeRequest
  :: ByteString
  -> IO Request
makeRequest request =
  Request
    <$> newTMVarIO request
    <*> newEmptyTMVarIO

-- | Attempt to update an outstanding request. Returns an @STM@ action that,
-- if it returns, indicates the socket will send the new request.
--
-- The action should typically be raced with the inner @STM@ action returned by
-- 'send'. If the former wins, it means the new request will be sent, and the
-- reply will correspond to the new request. If the latter wins, it means we
-- already received a reply to an outstanding request before updating it, and
-- subsequent calls to 'updateRequest' will now throw
-- 'Control.Exception.BlockedIndefinitelyOnSTM'.
updateRequest
  :: Request
  -> ByteString
  -> IO ( STM () )
updateRequest ( Request requestVar requestAcceptedVar ) request = do
  -- First, overwrite our old request.
  atomically do
    void ( tryTakeTMVar requestVar )
    putTMVar requestVar request

  -- But block until we're sure the socket has actually "accepted" our new
  -- request to send.
  pure ( takeTMVar requestAcceptedVar )


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
-- Sending occurs in two phases, both of which are @STM@ actions that can be
-- canceled (as by e.g. 'Control.Concurrent.STM.TVar.registerDelay').
--
-- The first phase is /send/, which blocks if the socket is currently being used
-- by another thread.
--
-- The second phase is /receive/, which blocks until the latest request sent is
-- replied to.
send
  :: ReqSocket
  -> Request
  -> IO ( STM ( STM ( Either Error ByteString ) ) )
send socket ( Request requestVar requestAcceptedVar ) = do
  responseVar :: TMVar ( Either Error ByteString ) <-
    newEmptyTMVarIO

  pure do
    putTMVar
      ( reqSocketVarsVar socket )
      ( Vars requestVar requestAcceptedVar responseVar )
    pure ( takeTMVar responseVar )
