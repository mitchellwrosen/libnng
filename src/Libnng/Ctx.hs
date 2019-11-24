module Libnng.Ctx where

import Foreign hiding (free)
import Foreign.C

import Libnng.Error
import Libnng.Types
import Libnng.Utils


ctx_close
  :: Ctx
  -> IO ( Either Error () )
ctx_close context =
  errnoToEither ( nng_ctx_close context )

foreign import ccall safe "static nng_ctx_close"
  nng_ctx_close
    :: Ctx
    -> IO CInt

ctx_open
  :: Socket
  -> IO ( Either Error Ctx )
ctx_open socket =
  alloca \ctxPtr ->
    onZero
      ( peek ctxPtr )
      ( nng_ctx_open ctxPtr socket )

foreign import ccall unsafe "static nng_ctx_open"
  nng_ctx_open
    :: Ptr Ctx
    -> Socket
    -> IO CInt

ctx_recv
  :: Ctx
  -> Aio
  -> IO ()
ctx_recv =
  nng_ctx_recv

foreign import ccall unsafe "static nng_ctx_recv"
  nng_ctx_recv
    :: Ctx
    -> Aio
    -> IO ()

ctx_send
  :: Ctx
  -> Aio
  -> IO ()
ctx_send =
  nng_ctx_send

foreign import ccall unsafe "static nng_ctx_send"
  nng_ctx_send
    :: Ctx
    -> Aio
    -> IO ()
