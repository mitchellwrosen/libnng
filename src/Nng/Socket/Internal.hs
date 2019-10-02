module Nng.Socket.Internal
  ( IsSocket(..)
  , closeDialer
  , closeListener
  , internalOpenDialer
  , internalOpenDialer_
  , internalOpenListener
  , internalOpenListener_
  , internalSendByteString
  , Fix(..)
  , RecvF(..)
  , internalRecvByteString
  ) where

import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.Posix.Types (Fd(..))
import qualified Data.ByteString.Unsafe as ByteString

import Nng.Address
import Nng.Error
import Nng.Prelude
import qualified Libnng


class IsSocket socket where
  close
    :: socket
    -> IO ( Either Error () )

  openDialer
    :: socket
    -> Address
    -> IO ( Either Error Libnng.Dialer )

  openDialer_
    :: socket
    -> Address
    -> IO ( Either Error () )

  openListener
    :: socket
    -> Address
    -> IO ( Either Error Libnng.Listener )

  openListener_
    :: socket
    -> Address
    -> IO ( Either Error () )

getRecvFd
  :: Libnng.Socket
  -> IO ( Either Error Fd )
getRecvFd socket =
  coerce
    ( errnoToError
        ( Libnng.getopt_int
            socket
            Libnng.oPT_RECVFD
        )
    )

getSendFd
  :: Libnng.Socket
  -> IO ( Either Error Fd )
getSendFd socket =
  coerce
    ( errnoToError
        ( Libnng.getopt_int
            socket
            Libnng.oPT_SENDFD
        )
    )

-- TODO dial flags
-- TODO dial type safety
internalOpenDialer
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error Libnng.Dialer )
internalOpenDialer socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.dial
              socket
              c_address
              0
        )
    )

internalOpenDialer_
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error () )
internalOpenDialer_ socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.dial_
              socket
              c_address
              0
        )
    )

closeDialer
  :: Libnng.Dialer
  -> IO ( Either Error () )
closeDialer dialer =
  errnoToError ( Libnng.dialer_close dialer )

-- TODO listen flags
-- TODO listen type safety
internalOpenListener
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error Libnng.Listener )
internalOpenListener socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.listen
              socket
              c_address
              0
        )
    )

internalOpenListener_
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error () )
internalOpenListener_ socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.listen_
              socket
              c_address
              0
        )
    )

closeListener
  :: Libnng.Listener
  -> IO ( Either Error () )
closeListener listener =
  errnoToError ( Libnng.listener_close listener )

-- TODO sendByteString_ flags
internalSendByteString
  :: Libnng.Socket
  -> ByteString
  -> IO ( Either Error () )
internalSendByteString socket bytes =
  loop >>= \case
    Left Error'TryAgain ->
      getSendFd socket >>= \case
        Left err ->
          pure ( Left err )

        Right sendFd -> do
          threadWaitRead sendFd
          loop

    Left err ->
      pure ( Left err )

    Right () ->
      pure ( Right () )

  where
    loop :: IO ( Either Error () )
    loop =
      errnoToError
        ( ByteString.unsafeUseAsCStringLen
            bytes
            ( \( ptr, len ) ->
                Libnng.send_unsafe
                  socket
                  ptr
                  ( fromIntegral len )
                  Libnng.fLAG_NONBLOCK
            )
        )

newtype Fix f
  = Fix { unFix :: f ( Fix f ) }

data RecvF a
  = RecvF'Error Error
  | RecvF'Again ( STM () ) ( IO () ) ( IO a )
  | RecvF'Ok ByteString

-- TODO recvByteString_ async exception safety
internalRecvByteString
  :: Libnng.Socket
  -> IO ( Fix RecvF )
internalRecvByteString socket =
  loop

  where
    loop :: IO ( Fix RecvF )
    loop =
      attempt >>= \case
        Left Error'TryAgain ->
          getRecvFd socket >>= \case
            Left err ->
              pure ( Fix ( RecvF'Error err ) )

            Right recvFd -> do
              ( ready, uninterested ) <-
                threadWaitReadSTM recvFd

              pure
                ( Fix
                    ( RecvF'Again
                        ready
                        uninterested
                        loop
                    )
                )

        Left err ->
          pure ( Fix ( RecvF'Error err ) )

        Right bytes ->
          pure ( Fix ( RecvF'Ok bytes ) )

    attempt :: IO ( Either Error ByteString )
    attempt =
      errnoToError
        ( alloca \ptrPtr ->
            alloca \lenPtr ->
              doRecv ptrPtr lenPtr >>= \case
                Left err ->
                  pure ( Left err )

                Right () -> do
                  ptr :: Ptr Word8 <-
                    peek ptrPtr

                  len :: CSize <-
                    peek lenPtr

                  Right <$>
                    ByteString.unsafePackCStringFinalizer
                      ptr
                      ( fromIntegral len )
                      ( Libnng.free ptr len )
        )

    doRecv
      :: Ptr ( Ptr Word8 )
      -> Ptr CSize
      -> IO ( Either CInt () )
    doRecv ptrPtr lenPtr =
      Libnng.recv_unsafe
        socket
        ptrPtr
        lenPtr
        ( Libnng.fLAG_ALLOC .|. Libnng.fLAG_NONBLOCK )
