module Nng.Socket.Internal
  ( IsSocket(..)
  , internalOpenDialer
  , internalOpenDialer_
  , internalOpenListener
  , internalOpenListener_
  , internalSendByteString
  , internalRecvByteString
  ) where

import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.Posix.Types (Fd(..))
import qualified Data.ByteString.Unsafe as ByteString

import Libnng (Error(..))
import Nng.Address
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
  coerce ( Libnng.getopt_int socket Libnng.oPT_RECVFD )

getSendFd
  :: Libnng.Socket
  -> IO ( Either Error Fd )
getSendFd socket =
  coerce ( Libnng.getopt_int socket Libnng.oPT_SENDFD )

-- TODO dial flags
-- TODO dial type safety
internalOpenDialer
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error Libnng.Dialer )
internalOpenDialer socket address =
  addressAsCString
    address
    ( \c_address ->
        Libnng.dial
          socket
          c_address
          0
    )

internalOpenDialer_
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error () )
internalOpenDialer_ socket address =
  addressAsCString
    address
    ( \c_address ->
        Libnng.dial_
          socket
          c_address
          0
    )

-- TODO listen flags
-- TODO listen type safety
internalOpenListener
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error Libnng.Listener )
internalOpenListener socket address =
  addressAsCString
    address
    ( \c_address ->
        Libnng.listen
          socket
          c_address
          0
    )

internalOpenListener_
  :: Libnng.Socket
  -> Address
  -> IO ( Either Error () )
internalOpenListener_ socket address =
  addressAsCString
    address
    ( \c_address ->
        Libnng.listen_
          socket
          c_address
          0
    )

-- TODO sendByteString_ flags
internalSendByteString
  :: Libnng.Socket
  -> ByteString
  -> IO ( Either Error () )
internalSendByteString socket bytes =
  loop >>= \case
    Left TryAgain ->
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
      ByteString.unsafeUseAsCStringLen
        bytes
        ( \( ptr, len ) ->
            Libnng.send_unsafe
              socket
              ptr
              ( fromIntegral len )
              Libnng.fLAG_NONBLOCK
        )

-- TODO internalRecvByteString_ async exception safety
internalRecvByteString
  :: Libnng.Socket
  -> IO ( Either Error ByteString )
internalRecvByteString socket =
  loop

  where
    loop :: IO ( Either Error ByteString )
    loop =
      attempt >>= \case
        Left TryAgain ->
          getRecvFd socket >>= \case
            Left err ->
              pure ( Left err )

            Right recvFd -> do
              ( ready, _uninterested ) <- threadWaitReadSTM recvFd
              atomically ready
              loop

        Left err ->
          pure ( Left err )

        Right bytes ->
          pure ( Right bytes )

    attempt :: IO ( Either Error ByteString )
    attempt =
      alloca \ptrPtr ->
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

    doRecv
      :: Ptr ( Ptr Word8 )
      -> Ptr CSize
      -> IO ( Either Error () )
    doRecv ptrPtr lenPtr =
      Libnng.recv_unsafe
        socket
        ptrPtr
        lenPtr
        ( Libnng.fLAG_ALLOC .|. Libnng.fLAG_NONBLOCK )
