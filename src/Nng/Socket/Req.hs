module Nng.Socket.Req
  ( ReqSocket
  , open
  , sendByteString
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
  , reqSocketVarsVar :: TMVar ( Vars ByteString )
  }

data Vars a
  = Vars
      ( TMVar a ) -- requests come in
      ( TMVar ( Either Error a ) ) -- responses go out

newtype Request a
  = Request ( TMVar a )

makeRequest
  :: a
  -> IO ( Request a )
makeRequest x =
  coerce ( newTMVarIO x )

updateRequest
  :: Request a
  -> a
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
      varsVar :: TMVar ( Vars ByteString ) <-
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
  -> TMVar ( Vars ByteString )
  -> IO ()
managerThread socket varsVar =
  forever do
    Vars requestVar responseVar <-
      atomically ( takeTMVar varsVar )

    response :: Either Error ByteString <-
      handleRequest socket ( takeTMVar requestVar )

    atomically ( putTMVar responseVar response )

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

sendByteString
  :: ReqSocket
  -> Request ByteString
  -> IO ( STM ( Either Error ByteString ) )
sendByteString socket ( Request requestVar ) = do
  responseVar :: TMVar ( Either Error ByteString ) <-
    newEmptyTMVarIO

  atomically
    ( putTMVar
        ( reqSocketVarsVar socket )
        ( Vars requestVar responseVar )
    )

  pure ( takeTMVar responseVar )
