module Libnng.Aio where

import Foreign hiding (free)
import Foreign.C

import Libnng.Error
import Libnng.Types
import Libnng.Utils


foreign import ccall unsafe "static nng_aio_abort"
  nng_aio_abort
    :: Aio
    -> CInt
    -> IO ()

-- | https://nanomsg.github.io/nng/man/v1.1.0/nng_aio_alloc.3
aio_alloc
  :: FunPtr ( Ptr a -> IO () ) -- ^ Callback
  -> Ptr a -- ^ Callback argument
  -> IO ( Either Error Aio )
aio_alloc callback arg =
  alloca \aioPtr ->
    onZero
      ( peek aioPtr )
      ( nng_aio_alloc aioPtr callback arg )

foreign import ccall unsafe "static nng_aio_alloc"
  nng_aio_alloc
    :: Ptr Aio
    -> FunPtr ( Ptr a -> IO () )
    -> Ptr a
    -> IO CInt


foreign import ccall unsafe "static nng_aio_begin"
  nng_aio_begin
    :: Aio
    -> IO CBool

foreign import ccall unsafe "static nng_aio_count"
  nng_aio_count
    :: Aio
    -> IO CSize

foreign import ccall unsafe "static nng_aio_defer"
  nng_aio_defer
    :: Aio
    -> FunPtr ( Aio -> Ptr a -> CInt -> IO () )
    -> Ptr a
    -> IO ()

foreign import ccall unsafe "static nng_aio_finish"
  nng_aio_finish
    :: Aio
    -> CInt
    -> IO ()

foreign import ccall safe "static nng_aio_free"
  nng_aio_free
    :: Aio
    -> IO ()

foreign import ccall unsafe "static nng_aio_get_input"
  nng_aio_get_input
    :: Aio
    -> CUInt
    -> IO ( Ptr a )

foreign import ccall unsafe "static nng_aio_get_msg"
  nng_aio_get_msg
    :: Aio
    -> IO Msg

foreign import ccall unsafe "static nng_aio_get_output"
  nng_aio_get_output
    :: Aio
    -> CUInt
    -> IO ( Ptr a )

foreign import ccall unsafe "static nng_aio_result"
  nng_aio_result
    :: Aio
    -> IO CInt

foreign import ccall unsafe "static nng_aio_set_input"
  nng_aio_set_input
    :: Aio
    -> CUInt
    -> Ptr a
    -> IO ()

-- TODO nng_aio_set_iov

foreign import ccall unsafe "static nng_aio_set_msg"
  nng_aio_set_msg
    :: Aio
    -> Msg
    -> IO ()

foreign import ccall unsafe "static nng_aio_set_output"
  nng_aio_set_output
    :: Aio
    -> CUInt
    -> Ptr a
    -> IO ()

foreign import ccall unsafe "static nng_aio_set_timeout"
  nng_aio_set_timeout
    :: Aio
    -> Duration
    -> IO ()

foreign import ccall safe "static nng_aio_stop"
  nng_aio_stop
    :: Aio
    -> IO ()

foreign import ccall safe "static nng_aio_wait"
  nng_aio_wait
    :: Aio
    -> IO ()
