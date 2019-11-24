module Libnng
  ( Aio
  , Ctx(..)
  , Error(..)
  , Dialer(..)
  , Listener(..)
  , Socket(..)
    -- * Common functions
  -- , alloc
  , free
  -- , strdup
  , strerror
  -- , strfree
  , version
    -- * Socket functions
  , close
  -- , getopt
  , getopt_int
  , recv
  , recv_unsafe
  , send
  , send_unsafe
  -- , setopt
    -- * Connection management
  , dial
  , dial_unsafe
  , dial_
  , dial_unsafe_
  , dialer_close
  , dialer_create
  -- , dialer_getopt
  , dialer_setopt_bool
  , dialer_setopt_int
  , dialer_start
  , dialer_start_unsafe
  , listen
  , listen_
  , listener_close
  , listener_create
  -- , listener_getopt
  -- , listener_setopt
  , listener_start
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
    -- ** Message header handling
  -- , msg_header
  -- , msg_header_append
  -- , msg_header_chop
  -- , msg_header_clear
  -- , msg_header_insert
  -- , msg_header_len
  -- , msg_header_trim
    -- * Asynchronous operations
  -- , aio_abort
  , aio_alloc
  -- , aio_begin
  -- , aio_cancel
  -- , aio_count
  -- , aio_defer
  -- , aio_finish
  -- , aio_free
  -- , aio_get_input
  -- , aio_get_msg
  -- , aio_get_output
  -- , aio_result
  -- , aio_set_input
  -- , aio_set_iov
  -- , aio_set_msg
  -- , aio_set_output
  -- , aio_set_timeout
  -- , aio_stop
  -- , aio_wait
  -- , recv_aio
  -- , send_aio
  -- , sleep_aio
    -- * Protocols
  , bus0_open
  , pair0_open
  , pair1_open
  , pub0_open
  , pull0_open
  , push0_open
  , rep0_open
  , req0_open
  , respondent0_open
  , sub0_open
  , surveyor0_open
    -- * Transports
  -- , inproregister
  -- , ipregister
  -- , tcp_register
  -- , tls_register
  -- , ws_register
  -- , wss_register
  -- , zt_register
    -- * Protocol contexts
  , ctx_close
  -- , ctx_getopt
  , ctx_open
  , ctx_recv
  , ctx_send
  -- , ctx_setopt
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
    -- * Flags
  , fLAG_ALLOC
  , fLAG_NONBLOCK
    -- * Options
  , oPT_RECVFD
  , oPT_SENDFD
  ) where

import Foreign hiding (free)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Libnng.Aio
import Libnng.Ctx
import Libnng.Error
import Libnng.Foreign
import Libnng.Socket
import Libnng.Types


free
  :: Ptr a
  -> CSize
  -> IO ()
free =
  nng_free

strerror
  :: CInt
  -> CString
strerror =
  nng_strerror

version :: CString
version =
  nng_version


--------------------------------------------------------------------------------
-- Flags
--------------------------------------------------------------------------------

fLAG_ALLOC :: CInt
fLAG_ALLOC =
  1

fLAG_NONBLOCK :: CInt
fLAG_NONBLOCK =
  2


--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

oPT_RECVFD :: CString
oPT_RECVFD =
  unsafePerformIO ( newCAString "recv-fd" )
{-# NOINLINE oPT_RECVFD #-}

oPT_SENDFD :: CString
oPT_SENDFD =
  unsafePerformIO ( newCAString "send-fd" )
{-# NOINLINE oPT_SENDFD #-}
