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
  { reqSocketRequestVar :: MVar Request
  , reqSocketSocket :: Libnng.Socket
  , reqSocketThread :: ThreadId
  }

data Request
  = Request ByteString ( TMVar ( Either Error ByteString ) )

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
  internalClose ( reqSocketSocket socket )


--------------------------------------------------------------------------------
-- Open
--------------------------------------------------------------------------------

open :: IO ( Either Error ReqSocket )
open =
  Libnng.req0_open >>= \case
    Left err ->
      pure ( Left ( cintToError err ) )

    Right socket -> do
      requestVar :: MVar Request <-
        newEmptyMVar

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
  -> MVar Request
  -> IO ()
managerThread socket requestVar =
  forever do
    Request request responseVar <-
      takeMVar requestVar

    response :: Either Error ByteString <-
      handleRequest
        socket
        request

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
--   Socket ready     => [Wait for response]
handleRequest
  :: Libnng.Socket
  -> ByteString
  -> IO ( Either Error ByteString )
handleRequest socket request =
  internalSendByteString socket request >>= \case
    Left err ->
      pure ( Left err )

    Right () ->
      internalRecvByteString socket


--------------------------------------------------------------------------------
-- Send
--------------------------------------------------------------------------------

-- | Send a request on a @req@ socket and receive its reply.
send
  :: ReqSocket
  -> ByteString
  -> IO ( Either Error ByteString )
send socket request = do
  responseVar :: TMVar ( Either Error ByteString ) <-
    newEmptyTMVarIO

  putMVar
    ( reqSocketRequestVar socket )
    ( Request request responseVar )

  atomically ( takeTMVar responseVar )
