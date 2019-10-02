module Nng.Socket.Req
  ( ReqSocket
  , open
  , sendByteString
  ) where

import Nng.Error
import Nng.Prelude
import Nng.Socket.Internal
import qualified Libnng


data ReqSocket
  = ReqSocket
  { reqSocketRequestVar :: TMVar Request
  , reqSocketSocket :: Libnng.Socket
  , reqSocketThread :: ThreadId
  }

data Request
  = Request
  { requestRequestVar :: TMVar ByteString -- full when request is made
  , requestResponseVar :: TMVar ( Either Error ByteString )
  }

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
      requestVar :: TMVar Request <-
        newEmptyTMVarIO

      managerThreadId :: ThreadId <-
        forkIO ( unsafeUnmask ( managerThread socket requestVar ) )

      pure
        ( Right ReqSocket
            { reqSocketRequestVar = requestVar
            , reqSocketSocket = socket
            , reqSocketThread = managerThreadId
            }
        )

managerThread
  :: Libnng.Socket
  -> TMVar Request
  -> IO ()
managerThread socket requestVar =
  forever do
    request :: Request <-
      atomically ( takeTMVar requestVar )

    response :: Either Error ByteString <-
      handleRequest
        socket
        ( takeTMVar ( requestRequestVar request ) )

    atomically ( putTMVar ( requestResponseVar request ) response )

-- Send request
--   Error   => Return
--   Success => 3
-- Receive response
--   Error            => Return
--   Success          => Return
--   Socket not ready =>
-- 4 Wait
--     Take request => 2
--     Ready        => 3
handleRequest
  :: Libnng.Socket
  -> STM ByteString
  -> IO ( Either Error ByteString )
handleRequest socket takeRequest =
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
                exchange newRequest

          in
            join ( atomically ( action1 <|> action2 ) )


--------------------------------------------------------------------------------
-- Send
--------------------------------------------------------------------------------

-- TODO more type safe interface to making requests (that TMVar could be empty)
sendByteString
  :: ReqSocket
  -> TMVar ByteString
  -> IO ( STM ( Either Error ByteString ) )
sendByteString socket requestVar = do
  responseVar :: TMVar ( Either Error ByteString ) <-
    newEmptyTMVarIO

  atomically
    ( putTMVar
        ( reqSocketRequestVar socket )
        ( Request
            { requestRequestVar = requestVar
            , requestResponseVar = responseVar
            }
        )
    )

  pure ( takeTMVar responseVar )
