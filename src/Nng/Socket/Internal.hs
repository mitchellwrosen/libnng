module Nng.Socket.Internal
  ( IsSocket(..)
  , openDialer
  , openDialer_
  , closeDialer
  , openListener
  , openListener_
  , closeListener
  , sendByteString_
  , recvByteString_
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
openDialer
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error Libnng.Dialer )
openDialer socket address =
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

openDialer_
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error () )
openDialer_ socket address =
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
openListener
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error Libnng.Listener )
openListener socket address =
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

openListener_
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error () )
openListener_ socket address =
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

sendByteString_
  :: Libnng.Socket
  -> ByteString
  -> IO ( Either Error () )
sendByteString_ socket bytes =
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

recvByteString_
  :: Libnng.Socket
  -> IO ( Either Error ByteString )
recvByteString_ socket =
  loop >>= \case
    Left Error'TryAgain ->
      getRecvFd socket >>= \case
        Left err ->
          pure ( Left err )

        Right recvFd -> do
          threadWaitRead recvFd
          loop

    Left err ->
      pure ( Left err )

    Right bytes ->
      pure ( Right bytes )

  where
    loop :: IO ( Either Error ByteString )
    loop =
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
