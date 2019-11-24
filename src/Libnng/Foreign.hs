module Libnng.Foreign where

import Foreign hiding (free)
import Foreign.C

import Libnng.Types


foreign import ccall unsafe "static nng_free"
  nng_free
    :: Ptr a
    -> CSize
    -> IO ()

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

foreign import ccall "static nng_strerror"
  nng_strerror
    :: CInt
    -> CString

foreign import ccall unsafe "static nng_version"
  nng_version
    :: CString


-- foreign import ccall "wrapper"
--   makeAioCallback
--     :: ( Ptr a -> IO () )
--     -> IO ( FunPtr ( Ptr a -> IO () ) )
