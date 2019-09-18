{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Libnng
  ( NngDialer
  , NngListener
  , NngSocket
  , nng_close
  , nng_dial
  , nng_dial_
  , nng_dialer_close
  , nng_listen
  , nng_listen_
  , nng_rep0_open
  , nng_req0_open
  , nng_send
  , nng_send_unsafe
  , nng_socket_id
  , nng_strerror
  , nng_version
  ) where

import Data.Coerce (coerce)
import Data.Word (Word32)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable


newtype NngDialer
  = NngDialer Word32
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype NngListener
  = NngListener Word32
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype NngSocket
  = NngSocket Word32
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )


nng_close
  :: NngSocket
  -> IO ( Either CInt () )
nng_close socket =
  c_nng_close socket >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

nng_dial
  :: NngSocket
  -> CString
  -> CInt
  -> IO ( Either CInt NngDialer )
nng_dial socket url flags =
  allocaBytes 4 \dialerPtr -> do
    c_nng_dial socket url dialerPtr flags >>= \case
      0 -> Right <$> peek dialerPtr
      n -> pure ( Left n )

nng_dial_
  :: NngSocket
  -> CString
  -> CInt
  -> IO ( Either CInt () )
nng_dial_ socket url flags =
  c_nng_dial socket url nullPtr flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

nng_dialer_close
  :: NngDialer
  -> IO ( Either CInt () )
nng_dialer_close dialer =
  c_nng_dialer_close dialer >>= \case
    0 ->
      pure ( Right () )

    n ->
      pure ( Left n )

nng_listen
  :: NngSocket
  -> CString
  -> CInt
  -> IO ( Either CInt NngListener )
nng_listen socket url flags =
  allocaBytes 4 \listenerPtr ->
    c_nng_listen socket url listenerPtr flags >>= \case
      0 -> Right <$> peek listenerPtr
      n -> pure ( Left n )

nng_listen_
  :: NngSocket
  -> CString
  -> CInt
  -> IO ( Either CInt () )
nng_listen_ socket url flags =
  c_nng_listen socket url nullPtr flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

nng_rep0_open :: IO ( Either CInt NngSocket )
nng_rep0_open =
  allocaBytes 4 \socketPtr ->
    c_nng_rep0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

nng_req0_open :: IO ( Either CInt NngSocket )
nng_req0_open =
  allocaBytes 4 \socketPtr ->
    c_nng_req0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

nng_send
  :: NngSocket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either CInt () )
nng_send socket data_ size flags =
  c_nng_send socket data_ size flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

nng_send_unsafe
  :: NngSocket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either CInt () )
nng_send_unsafe socket data_ size flags =
  c_nng_send_unsafe socket data_ size flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

nng_socket_id
  :: NngSocket
  -> Word32
nng_socket_id =
  coerce

nng_strerror
  :: CInt
  -> CString
nng_strerror =
  c_nng_strerror

nng_version :: CString
nng_version =
  c_nng_version


--------------------------------------------------------------------------------
-- Foreign imports
-- TODO safe, unsafe, wha?
--------------------------------------------------------------------------------

foreign import ccall "nng_close" c_nng_close
  :: NngSocket
  -> IO CInt

foreign import ccall "nng_dial" c_nng_dial
  :: NngSocket
  -> CString
  -> Ptr NngDialer
  -> CInt
  -> IO CInt

foreign import ccall "nng_dialer_close" c_nng_dialer_close
  :: NngDialer
  -> IO CInt

foreign import ccall "nng_listen" c_nng_listen
  :: NngSocket
  -> CString
  -> Ptr NngListener
  -> CInt
  -> IO CInt

foreign import ccall safe "nng_recv" c_nng_recv
  :: NngSocket
  -> Ptr a
  -> Ptr CInt
  -> CInt
  -> IO CInt

foreign import ccall unsafe "nng_recv" c_nng_recv_unsafe
  :: NngSocket
  -> Ptr a
  -> Ptr CInt
  -> CInt
  -> IO CInt

foreign import ccall "nng_rep0_open" c_nng_rep0_open
  :: Ptr NngSocket
  -> IO CInt

foreign import ccall "nng_req0_open" c_nng_req0_open
  :: Ptr NngSocket
  -> IO CInt

foreign import ccall safe "nng_send" c_nng_send
  :: NngSocket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO CInt

foreign import ccall unsafe "nng_send" c_nng_send_unsafe
  :: NngSocket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO CInt

foreign import ccall "nng_strerror" c_nng_strerror
  :: CInt
  -> CString

foreign import ccall "nng_version" c_nng_version
  :: CString
