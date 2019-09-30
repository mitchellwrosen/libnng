module Libnng
  ( Aio
  , Ctx(..)
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
    -- * Message header handling
  -- , msg_header
  -- , msg_header_append
  -- , msg_header_chop
  -- , msg_header_clear
  -- , msg_header_insert
  -- , msg_header_len
  -- , msg_header_trim
    -- * Asynchronous operations
  -- , aio_abort
  -- , aio_alloc
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

import Libnng.Foreign
import Libnng.Types


-- aio_alloc
--   :: IO ()
--   -> IO ( Either CInt Aio )
-- aio_alloc callback =
--   alloca \aioPtr -> do
--     callback' <- makeAioCallback (const callback)
--     nng_aio_alloc aioPtr callback' nullPtr >>= \case
--       0 -> do
--         aio :: Ptr () <-
--           peek aioPtr

--         pure
--           ( Right Aio
--               { aioHandle = aio
--               , aioFree = freeHaskellFunPtr callback'
--               }
--           )

--       n ->
--         pure ( Left n )

bus0_open :: IO ( Either CInt Socket )
bus0_open =
  alloca \socketPtr ->
    nng_bus0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

close
  :: Socket
  -> IO ( Either CInt () )
close socket =
  errnoToEither ( nng_close socket )

ctx_close
  :: Ctx
  -> IO ( Either CInt () )
ctx_close context =
  errnoToEither ( nng_ctx_close context )

ctx_open
  :: Socket
  -> IO ( Either CInt Ctx )
ctx_open socket =
  alloca \ctxPtr ->
    nng_ctx_open ctxPtr socket >>= \case
      0 -> Right <$> peek ctxPtr
      n -> pure ( Left n )

ctx_recv
  :: Ctx
  -> Aio
  -> IO ()
ctx_recv =
  nng_ctx_recv

ctx_send
  :: Ctx
  -> Aio
  -> IO ()
ctx_send =
  nng_ctx_send

dial
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt Dialer )
dial socket url flags =
  alloca \dialerPtr ->
    nng_dial socket url dialerPtr flags >>= \case
      0 -> Right <$> peek dialerPtr
      n -> pure ( Left n )

dial_unsafe
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt Dialer )
dial_unsafe socket url flags =
  alloca \dialerPtr ->
    nng_dial_unsafe socket url dialerPtr flags >>= \case
      0 -> Right <$> peek dialerPtr
      n -> pure ( Left n )

dial_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt () )
dial_ socket url flags =
  errnoToEither ( nng_dial socket url nullPtr flags )

dial_unsafe_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt () )
dial_unsafe_ socket url flags =
  errnoToEither ( nng_dial_unsafe socket url nullPtr flags )

dialer_close
  :: Dialer
  -> IO ( Either CInt () )
dialer_close dialer =
  errnoToEither ( nng_dialer_close dialer )

dialer_create
  :: Socket
  -> CString
  -> IO ( Either CInt Dialer )
dialer_create socket url =
  alloca \dialerPtr ->
    nng_dialer_create dialerPtr socket url >>= \case
      0 -> Right <$> peek dialerPtr
      n -> pure ( Left n )

dialer_setopt_bool
  :: Dialer
  -> CString
  -> Bool
  -> IO ( Either CInt () )
dialer_setopt_bool dialer opt val =
  errnoToEither ( nng_dialer_setopt_bool dialer opt val )

dialer_setopt_int
  :: Dialer
  -> CString
  -> CInt
  -> IO ( Either CInt () )
dialer_setopt_int dialer opt val =
  errnoToEither ( nng_dialer_setopt_int dialer opt val )

dialer_start
  :: Dialer
  -> CInt
  -> IO ( Either CInt () )
dialer_start dialer flags =
  errnoToEither ( nng_dialer_start dialer flags )

dialer_start_unsafe
  :: Dialer
  -> CInt
  -> IO ( Either CInt () )
dialer_start_unsafe dialer flags =
  errnoToEither ( nng_dialer_start_unsafe dialer flags )

free
  :: Ptr a
  -> CSize
  -> IO ()
free =
  nng_free

getopt_int
  :: Socket
  -> CString
  -> IO ( Either CInt CInt )
getopt_int socket opt =
  alloca \valPtr ->
    nng_getopt_int socket opt valPtr >>= \case
      0 -> Right <$> peek valPtr
      n -> pure ( Left n )

listen
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt Listener )
listen socket url flags =
  alloca \listenerPtr ->
    nng_listen socket url listenerPtr flags >>= \case
      0 -> Right <$> peek listenerPtr
      n -> pure ( Left n )

listen_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either CInt () )
listen_ socket url flags =
  errnoToEither ( nng_listen socket url nullPtr flags )

listener_close
  :: Listener
  -> IO ( Either CInt () )
listener_close listener =
  errnoToEither ( nng_listener_close listener )

listener_create
  :: Socket
  -> CString
  -> IO ( Either CInt Listener )
listener_create socket url =
  alloca \listenerPtr ->
    nng_listener_create listenerPtr socket url >>= \case
      0 -> Right <$> peek listenerPtr
      n -> pure ( Left n )

listener_start
  :: Listener
  -> IO ( Either CInt () )
listener_start listener =
  errnoToEither ( nng_listener_start listener 0 )

pair0_open :: IO ( Either CInt Socket )
pair0_open =
  alloca \socketPtr ->
    nng_pair0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

pair1_open :: IO ( Either CInt Socket )
pair1_open =
  alloca \socketPtr ->
    nng_pair1_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

pub0_open :: IO ( Either CInt Socket )
pub0_open =
  alloca \socketPtr ->
    nng_pub0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

pull0_open :: IO ( Either CInt Socket )
pull0_open =
  alloca \socketPtr ->
    nng_pull0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

push0_open :: IO ( Either CInt Socket )
push0_open =
  alloca \socketPtr ->
    nng_push0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

recv
  :: Socket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either CInt () )
recv socket data_ size flags =
  errnoToEither ( nng_recv socket data_ size flags )

recv_unsafe
  :: Socket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either CInt () )
recv_unsafe socket data_ size flags =
  errnoToEither ( nng_recv_unsafe socket data_ size flags )

rep0_open :: IO ( Either CInt Socket )
rep0_open =
  alloca \socketPtr ->
    nng_rep0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

req0_open :: IO ( Either CInt Socket )
req0_open =
  alloca \socketPtr ->
    nng_req0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

respondent0_open :: IO ( Either CInt Socket )
respondent0_open =
  alloca \socketPtr ->
    nng_respondent0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

sub0_open :: IO ( Either CInt Socket )
sub0_open =
  alloca \socketPtr ->
    nng_sub0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

surveyor0_open :: IO ( Either CInt Socket )
surveyor0_open =
  alloca \socketPtr ->
    nng_surveyor0_open socketPtr >>= \case
      0 -> Right <$> peek socketPtr
      n -> pure ( Left n )

send
  :: Socket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either CInt () )
send socket data_ size flags =
  errnoToEither ( nng_send socket data_ size flags )

send_unsafe
  :: Socket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either CInt () )
send_unsafe socket data_ size flags =
  errnoToEither ( nng_send_unsafe socket data_ size flags )

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


--------------------------------------------------------------------------------
-- Misc. helpers
--------------------------------------------------------------------------------

errnoToEither
  :: IO CInt
  -> IO ( Either CInt () )
errnoToEither =
  fmap
    ( \case
        0 -> Right ()
        n -> Left n
    )
