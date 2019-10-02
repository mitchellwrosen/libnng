module Nng.Socket.Req
  ( ReqSocket
  , open
  ) where

import Nng.Error
import Nng.Prelude
import Nng.Socket.Internal
import qualified Libnng


data ReqSocket
  = ReqSocket
  { reqSocketRequestVar :: TMVar ( ByteString, TMVar ( Either Error ByteString ) )
  , reqSocketSocket :: Libnng.Socket
  , reqSocketThread :: ThreadId
  }

instance IsSocket ReqSocket where
  close = closeImpl

closeImpl
  :: ReqSocket
  -> IO ( Either Error () )
closeImpl socket = do
  killThread ( reqSocketThread socket )
  errnoToError ( Libnng.close ( reqSocketSocket socket ) )

open :: IO ( Either Error ReqSocket )
open =
  Libnng.req0_open >>= \case
    Left err ->
      pure ( Left ( cintToError err ) )

    Right socket -> do
      requestVar :: TMVar ( ByteString, TMVar ( Either Error ByteString ) ) <-
        newEmptyTMVarIO

      managerThread :: ThreadId <-
        forkIO do
          unsafeUnmask do
            forever do
              ( request, responseVar ) <-
                atomically ( readTMVar requestVar )

              sendByteString_ socket request >>= \case
                Left err ->
                  atomically ( putTMVar responseVar ( Left err ) )

                Right () ->
                  recvByteString_ socket >>= \case
                    Left err ->
                      atomically ( putTMVar responseVar ( Left err ) )

                    Right response ->
                      atomically ( putTMVar responseVar ( Right response ) )

      pure
        ( Right ReqSocket
            { reqSocketRequestVar = requestVar
            , reqSocketSocket = socket
            , reqSocketThread = managerThread
            }
        )
