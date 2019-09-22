{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}

module Libnng
  ( Aio
  , Dialer
  , Listener
  , Socket(..)
    -- * Common functions
  -- , alloc
  -- , free
  -- , strdup
  , strerror
  -- , strfree
  , version
    -- * Socket functions
  , close
  -- , getopt
  , recv
  , recv_unsafe
  , send
  , send_unsafe
  -- , setopt
    -- * Connection management
  , dial
  , dial_
  , dialer_close
  -- , dialer_create
  -- , dialer_getopt
  -- , dialer_id
  -- , dialer_setopt
  -- , dialer_start
  , listen
  , listen_
  -- , listener_close
  -- , listener_create
  -- , listener_getopt
  -- , listener_id
  -- , listener_setopt
  -- , listener_start
  -- , pipe_close
  -- , pipe_dialer
  -- , pipe_getopt
  -- , pipe_id
  -- , pipe_listener
  -- , pipe_notify
  -- , pipe_socket
    -- * Message handling functions
  -- , msg_alloc
  -- , msg_append
  -- , msg_body
  -- , msg_chop
  -- , msg_clear
  -- , msg_dup
  -- , msg_free
  -- , msg_get_pipe
  -- , msg_insert
  -- , msg_len
  -- , msg_realloc
  -- , msg_set_pipe
  -- , msg_trim
  -- , recvmsg
  -- , sendmsg
    -- * Message header handling
  -- , msg_header
  -- , msg_header_append
  -- , msg_header_chop
  -- , msg_header_clear
  -- , msg_header_insert
  -- , msg_header_len
  -- , msg_header_trim
    -- * Asynchronous operations
    -- * Protocols
  -- , bus_open
  -- , pair_open
  -- , pub_open
  -- , pull_open
  -- , push_open
  , rep0_open
  , req0_open
  -- , respondent_open
  -- , sub_open
  -- , surveyor_open
    -- * Transports
  -- , inproregister
  -- , ipregister
  -- , tcp_register
  -- , tls_register
  -- , ws_register
  -- , wss_register
  -- , zt_register
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

import Data.Word (Word32)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable


newtype Aio
  = Aio ( Ptr () )

newtype Dialer
  = Dialer Word32
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype Listener
  = Listener Word32
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype Socket
  = Socket { socket_id :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )


close
  :: Socket
  -> IO ( Either CInt () )
close socket =
  nng_close socket >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

dial
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt Dialer )
dial socket url flags =
  allocaBytes 4 \dialerPtr -> do
    nng_dial socket url dialerPtr flags >>= \case
      0 -> Right <$> peek dialerPtr
      n -> pure ( Left n )

dial_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt () )
dial_ socket url flags =
  nng_dial socket url nullPtr flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

dialer_close
  :: Dialer
  -> IO ( Either CInt () )
dialer_close dialer =
  nng_dialer_close dialer >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

listen
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt Listener )
listen socket url flags =
  allocaBytes 4 \listenerPtr ->
    nng_listen socket url listenerPtr flags >>= \case
      0 -> Right <$> peek listenerPtr
      n -> pure ( Left n )

listen_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt () )
listen_ socket url flags =
  nng_listen socket url nullPtr flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

recv
  :: Socket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either CInt () )
recv socket data_ size flags =
  nng_recv socket data_ size flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

recv_unsafe
  :: Socket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either CInt () )
recv_unsafe socket data_ size flags =
  nng_recv_unsafe socket data_ size flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

rep0_open :: IO ( Either CInt Socket )
rep0_open =
  allocaBytes 4 \socketPtr ->
    nng_rep0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

req0_open :: IO ( Either CInt Socket )
req0_open =
  allocaBytes 4 \socketPtr ->
    nng_req0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

send
  :: Socket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either CInt () )
send socket data_ size flags =
  nng_send socket data_ size flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

send_unsafe
  :: Socket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either CInt () )
send_unsafe socket data_ size flags =
  nng_send_unsafe socket data_ size flags >>= \case
    0 -> pure ( Right () )
    n -> pure ( Left n )

strerror
  :: CInt
  -> CString
strerror =
  nng_strerror

version :: CString
version =
  nng_version


--------------------------------------------------------------------------------
-- Foreign imports
-- TODO safe, unsafe, wha?
--------------------------------------------------------------------------------

foreign import ccall "nng_close"
  nng_close
    :: Socket
    -> IO CInt

foreign import ccall "nng_dial"
  nng_dial
    :: Socket
    -> CString
    -> Ptr Dialer
    -> CInt
    -> IO CInt

foreign import ccall "nng_dialer_close"
  nng_dialer_close
    :: Dialer
    -> IO CInt

foreign import ccall "nng_listen"
  nng_listen
    :: Socket
    -> CString
    -> Ptr Listener
    -> CInt
    -> IO CInt

foreign import ccall safe "nng_recv"
  nng_recv
    :: Socket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "nng_recv"
  nng_recv_unsafe
    :: Socket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "nng_rep0_open"
  nng_rep0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "nng_req0_open"
  nng_req0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall safe "nng_send"
  nng_send
    :: Socket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "nng_send"
  nng_send_unsafe
    :: Socket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

foreign import ccall "nng_strerror"
  nng_strerror
    :: CInt
    -> CString

foreign import ccall "nng_version"
  nng_version
    :: CString
