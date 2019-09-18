{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Libnng
  ( strerror
  , version
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text

foreign import ccall "nng_strerror" nng_strerror
  :: Int
  -> CString

foreign import ccall "nng_version" nng_version
  :: CString

strerror
  :: Int
  -> Text
strerror =
  Text.decodeUtf8 . unsafePerformIO . ByteString.packCString . nng_strerror
{-# NOINLINE strerror #-}

version :: ByteString
version =
  unsafePerformIO ( ByteString.packCString nng_version )
{-# NOINLINE version #-}
