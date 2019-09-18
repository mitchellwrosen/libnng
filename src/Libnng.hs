{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Libnng
  ( NngDialer(..)
  , NngListener(..)
  , NngSocket(..)
  , nng_close
  , nng_dial
  , nng_dialer_close
  , nng_listen
  , nng_rep0_open
  , nng_req0_open
  , nng_strerror
  , nng_version
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text


newtype NngDialer
  = NngDialer { unNngDialer :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype NngListener
  = NngListener { unNngListener :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype NngSocket
  = NngSocket { unNngSocket :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )


nng_close
  :: NngSocket
  -> IO Int
nng_close =
  c_nng_close

nng_dial
  :: NngSocket
  -> Text
  -> Int
  -> IO ( Either Int NngDialer )
nng_dial socket url flags =
  ByteString.useAsCString
    ( Text.encodeUtf8 url )
    \url2 ->
      allocaBytes 4 \dialerPtr -> do
        result :: Int <-
          c_nng_dial
            socket
            url2
            dialerPtr
            flags

        if result == 0
          then
            Right <$> peek dialerPtr

          else
            pure ( Left result )

nng_dialer_close
  :: NngDialer
  -> IO Int
nng_dialer_close =
  c_nng_dialer_close

nng_listen
  :: NngSocket
  -> Text
  -> Int
  -> IO ( Either Int NngListener )
nng_listen socket url flags =
  ByteString.useAsCString
    ( Text.encodeUtf8 url )
    \url2 ->
      allocaBytes 4 \listenerPtr -> do
        result :: Int <-
          c_nng_listen
            socket
            url2
            listenerPtr
            flags

        if result == 0
          then
            Right <$> peek listenerPtr

          else
            pure ( Left result )

nng_rep0_open
  :: IO ( Either Int NngSocket )
nng_rep0_open =
  allocaBytes 4 \socketPtr -> do
    result :: Int <-
      c_nng_rep0_open socketPtr

    if result == 0
      then
        Right <$> peek socketPtr

      else
        pure ( Left result )

nng_req0_open
  :: IO ( Either Int NngSocket )
nng_req0_open =
  allocaBytes 4 \socketPtr -> do
    result :: Int <-
      c_nng_req0_open socketPtr

    if result == 0
      then
        Right <$> peek socketPtr

      else
        pure ( Left result )

nng_strerror
  :: Int
  -> Text
nng_strerror =
  Text.decodeUtf8 . unsafePerformIO . ByteString.packCString . c_nng_strerror
{-# NOINLINE nng_strerror #-}

nng_version :: ByteString
nng_version =
  unsafePerformIO ( ByteString.packCString c_nng_version )
{-# NOINLINE nng_version #-}


--------------------------------------------------------------------------------
-- Foreign imports
-- TODO safe, unsafe, wha?
--------------------------------------------------------------------------------

foreign import ccall "nng_close" c_nng_close
  :: NngSocket
  -> IO Int

foreign import ccall "nng_dial" c_nng_dial
  :: NngSocket
  -> CString
  -> Ptr NngDialer
  -> Int
  -> IO Int

foreign import ccall "nng_dialer_close" c_nng_dialer_close
  :: NngDialer
  -> IO Int

foreign import ccall "nng_listen" c_nng_listen
  :: NngSocket
  -> CString
  -> Ptr NngListener
  -> Int
  -> IO Int

foreign import ccall "nng_rep0_open" c_nng_rep0_open
  :: Ptr NngSocket
  -> IO Int

foreign import ccall "nng_req0_open" c_nng_req0_open
  :: Ptr NngSocket
  -> IO Int

foreign import ccall "nng_strerror" c_nng_strerror
  :: Int
  -> CString

foreign import ccall "nng_version" c_nng_version
  :: CString
