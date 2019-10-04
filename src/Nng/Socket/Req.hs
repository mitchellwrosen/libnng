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
      ( TMVar ByteString ) -- requests come in
      ( TMVar ( Either Error ByteString ) ) -- responses go out

newtype Request
  = Request ( TMVar ByteString )

makeRequest
  :: ByteString
  -> IO Request
makeRequest x =
  coerce ( newTMVarIO x )

updateRequest
  :: Request
  -> ByteString
  -> STM ()
updateRequest ( Request var ) x = do
  void ( tryTakeTMVar var )
  putTMVar var x

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
    Vars requestVar responseVar <-
      atomically ( takeTMVar varsVar )

    response :: Either Error ByteString <-
      handleRequest socket ( takeTMVar requestVar )

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

send
  :: ReqSocket
  -> Request
  -> IO ( STM ( Either Error ByteString ) )
send socket ( Request requestVar ) = do
  responseVar :: TMVar ( Either Error ByteString ) <-
    newEmptyTMVarIO

  atomically
    ( putTMVar
        ( reqSocketVarsVar socket )
        ( Vars requestVar responseVar )
    )

  pure ( takeTMVar responseVar )
