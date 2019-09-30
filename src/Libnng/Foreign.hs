module Libnng.Foreign where

import Foreign hiding (free)
import Foreign.C

import Libnng.Types


foreign import ccall unsafe "nng_aio_alloc"
  nng_aio_alloc
    :: Ptr Aio
    -> FunPtr ( Ptr a -> IO () )
    -> Ptr a
    -> IO CInt

foreign import ccall unsafe "static nng_bus0_open"
  nng_bus0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_close"
  nng_close
    :: Socket
    -> IO CInt

foreign import ccall safe "static nng_ctx_close"
  nng_ctx_close
    :: Ctx
    -> IO CInt

foreign import ccall unsafe "static nng_ctx_open"
  nng_ctx_open
    :: Ptr Ctx
    -> Socket
    -> IO CInt

foreign import ccall unsafe "static nng_ctx_recv"
  nng_ctx_recv
    :: Ctx
    -> Aio
    -> IO ()

foreign import ccall unsafe "static nng_ctx_send"
  nng_ctx_send
    :: Ctx
    -> Aio
    -> IO ()

foreign import ccall safe "static nng_dial"
  nng_dial
    :: Socket
    -> CString
    -> Ptr Dialer
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_dial"
  nng_dial_unsafe
    :: Socket
    -> CString
    -> Ptr Dialer
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_dialer_close"
  nng_dialer_close
    :: Dialer
    -> IO CInt

foreign import ccall unsafe "static nng_dialer_create"
  nng_dialer_create
    :: Ptr Dialer
    -> Socket
    -> CString
    -> IO CInt

foreign import ccall unsafe "static nng_dialer_setopt_bool"
  nng_dialer_setopt_bool
    :: Dialer
    -> CString
    -> Bool
    -> IO CInt

foreign import ccall unsafe "static nng_dialer_setopt_int"
  nng_dialer_setopt_int
    :: Dialer
    -> CString
    -> CInt
    -> IO CInt

foreign import ccall safe "static nng_dialer_start"
  nng_dialer_start
    :: Dialer
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_dialer_start"
  nng_dialer_start_unsafe
    :: Dialer
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_free"
  nng_free
    :: Ptr a
    -> CSize
    -> IO ()

foreign import ccall unsafe "static nng_getopt_int"
  nng_getopt_int
    :: Socket
    -> CString
    -> Ptr CInt
    -> IO CInt

foreign import ccall unsafe "static nng_listen"
  nng_listen
    :: Socket
    -> CString
    -> Ptr Listener
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_listener_close"
  nng_listener_close
    :: Listener
    -> IO CInt

foreign import ccall unsafe "static nng_listener_create"
  nng_listener_create
    :: Ptr Listener
    -> Socket
    -> CString
    -> IO CInt

foreign import ccall unsafe "static nng_listener_start"
  nng_listener_start
    :: Listener
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_msg_alloc"
  nng_msg_alloc
    :: Ptr Msg
    -> CSize
    -> IO CInt

foreign import ccall unsafe "static nng_msg_append"
  nng_msg_append
    :: Msg
    -> Ptr a
    -> CSize
    -> IO CInt

foreign import ccall unsafe "static nng_msg_body"
  nng_msg_body
    :: Msg
    -> IO ( Ptr a )

foreign import ccall unsafe "static nng_msg_chop"
  nng_msg_chop
    :: Msg
    -> CSize
    -> IO CInt

foreign import ccall unsafe "static nng_msg_clear"
  nng_msg_clear
    :: Msg
    -> IO ()

foreign import ccall unsafe "static nng_msg_dup"
  nng_msg_dup
    :: Ptr Msg
    -> Msg
    -> IO CInt

foreign import ccall unsafe "static nng_msg_free"
  nng_msg_free
    :: Msg
    -> IO ()

foreign import ccall unsafe "static nng_msg_get_pipe"
  nng_msg_get_pipe
    :: Msg
    -> IO Pipe

foreign import ccall unsafe "static nng_msg_insert"
  nng_msg_insert
    :: Msg
    -> Ptr a
    -> CSize
    -> IO CInt

foreign import ccall unsafe "static nng_msg_len"
  nng_msg_len
    :: Msg
    -> IO CSize

foreign import ccall unsafe "static nng_msg_realloc"
  nng_msg_realloc
    :: Msg
    -> CSize
    -> IO CInt

foreign import ccall unsafe "static nng_msg_set_pipe"
  nng_msg_set_pipe
    :: Msg
    -> Pipe
    -> IO ()

foreign import ccall unsafe "static nng_msg_trim"
  nng_msg_trim
    :: Msg
    -> CSize
    -> IO CInt

foreign import ccall unsafe "static nng_pair0_open"
  nng_pair0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_pair1_open"
  nng_pair1_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_pub0_open"
  nng_pub0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_pull0_open"
  nng_pull0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_push0_open"
  nng_push0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall safe "static nng_recv"
  nng_recv
    :: Socket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_recv"
  nng_recv_unsafe
    :: Socket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

foreign import ccall safe "static nng_recvmsg"
  nng_recvmsg
    :: Socket
    -> Ptr Msg
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_recvmsg"
  nng_recvmsg_unsafe
    :: Socket
    -> Ptr Msg
    -> CInt
    -> IO CInt

foreign import ccall safe "static nng_sendmsg"
  nng_sendmsg
    :: Socket
    -> Msg
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_sendmsg"
  nng_sendmsg_unsafe
    :: Socket
    -> Msg
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_rep0_open"
  nng_rep0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_req0_open"
  nng_req0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_respondent0_open"
  nng_respondent0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall safe "static nng_send"
  nng_send
    :: Socket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_send"
  nng_send_unsafe
    :: Socket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

foreign import ccall "static nng_strerror"
  nng_strerror
    :: CInt
    -> CString

foreign import ccall unsafe "static nng_sub0_open"
  nng_sub0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_surveyor0_open"
  nng_surveyor0_open
    :: Ptr Socket
    -> IO CInt

foreign import ccall unsafe "static nng_version"
  nng_version
    :: CString


-- foreign import ccall "wrapper"
--   makeAioCallback
--     :: ( Ptr a -> IO () )
--     -> IO ( FunPtr ( Ptr a -> IO () ) )
