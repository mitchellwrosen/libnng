{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}

module Libnng
  ( NngAio
  , NngDialer
  , NngListener
  , NngSocket(..)
    -- * Common functions
  -- , nng_alloc
  -- , nng_free
  -- , nng_strdup
  , nng_strerror
  -- , nng_strfree
  , nng_version
    -- * Socket functions
  , nng_close
  -- , nng_getopt
  , nng_recv
  , nng_recv_unsafe
  , nng_send
  , nng_send_unsafe
  -- , nng_setopt
    -- * Connection management
  , nng_dial
  , nng_dial_
  , nng_dialer_close
  -- , nng_dialer_create
  -- , nng_dialer_getopt
  -- , nng_dialer_id
  -- , nng_dialer_setopt
  -- , nng_dialer_start
  , nng_listen
  , nng_listen_
  -- , nng_listener_close
  -- , nng_listener_create
  -- , nng_listener_getopt
  -- , nng_listener_id
  -- , nng_listener_setopt
  -- , nng_listener_start
  -- , nng_pipe_close
  -- , nng_pipe_dialer
  -- , nng_pipe_getopt
  -- , nng_pipe_id
  -- , nng_pipe_listener
  -- , nng_pipe_notify
  -- , nng_pipe_socket
    -- * Message handling functions
  -- , nng_msg_alloc
  -- , nng_msg_append
  -- , nng_msg_body
  -- , nng_msg_chop
  -- , nng_msg_clear
  -- , nng_msg_dup
  -- , nng_msg_free
  -- , nng_msg_get_pipe
  -- , nng_msg_insert
  -- , nng_msg_len
  -- , nng_msg_realloc
  -- , nng_msg_set_pipe
  -- , nng_msg_trim
  -- , nng_recvmsg
  -- , nng_sendmsg
    -- * Message header handling
  -- , nng_msg_header
  -- , nng_msg_header_append
  -- , nng_msg_header_chop
  -- , nng_msg_header_clear
  -- , nng_msg_header_insert
  -- , nng_msg_header_len
  -- , nng_msg_header_trim
    -- * Asynchronous operations
    -- * Protocols
  -- , nng_bus_open
  -- , nng_pair_open
  -- , nng_pub_open
  -- , nng_pull_open
  -- , nng_push_open
  , nng_rep0_open
  , nng_req0_open
  -- , nng_respondent_open
  -- , nng_sub_open
  -- , nng_surveyor_open
    -- * Transports
  -- , nng_inproc_register
  -- , nng_ipc_register
  -- , nng_tcp_register
  -- , nng_tls_register
  -- , nng_ws_register
  -- , nng_wss_register
  -- , nng_zt_register
    -- * Protocol contexts
    -- * Statistics
    -- * URL object
    -- * Supplemental API
    -- * Supplemental TCP
    -- * Supplemental IPC
    -- * HTTP support
    -- ** Common HTTP functions
    -- ** HTTP client functions
    -- ** HTTP server functions
    -- * TLS configuration objects
  ) where

import Data.Coerce (coerce)
import Data.Word (Word32)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable


newtype NngAio
  = NngAio ( Ptr () )

newtype NngDialer
  = NngDialer Word32
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype NngListener
  = NngListener Word32
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype NngSocket
  = NngSocket { nng_socket_id :: Word32 }
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
    0 -> pure ( Right () )
    n -> pure ( Left n )

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

nng_recv
  :: NngSocket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either CInt () )
nng_recv socket data_ size flags =
  c_nng_recv socket data_ size flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

nng_recv_unsafe
  :: NngSocket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either CInt () )
nng_recv_unsafe socket data_ size flags =
  c_nng_recv_unsafe socket data_ size flags >>= \case
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

foreign import ccall "nng_close"
  c_nng_close
    :: NngSocket
    -> IO CInt

foreign import ccall "nng_dial"
  c_nng_dial
    :: NngSocket
    -> CString
    -> Ptr NngDialer
    -> CInt
    -> IO CInt

foreign import ccall "nng_dialer_close"
  c_nng_dialer_close
    :: NngDialer
    -> IO CInt

foreign import ccall "nng_listen"
  c_nng_listen
    :: NngSocket
    -> CString
    -> Ptr NngListener
    -> CInt
    -> IO CInt

foreign import ccall safe "nng_recv"
  c_nng_recv
    :: NngSocket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "nng_recv"
  c_nng_recv_unsafe
    :: NngSocket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "nng_rep0_open"
  c_nng_rep0_open
    :: Ptr NngSocket
    -> IO CInt

foreign import ccall unsafe "nng_req0_open"
  c_nng_req0_open
    :: Ptr NngSocket
    -> IO CInt

foreign import ccall safe "nng_send"
  c_nng_send
    :: NngSocket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "nng_send"
  c_nng_send_unsafe
    :: NngSocket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

foreign import ccall "nng_strerror"
  c_nng_strerror
    :: CInt
    -> CString

foreign import ccall "nng_version"
  c_nng_version
    :: CString
