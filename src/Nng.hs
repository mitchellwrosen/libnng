{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nng
  ( Socket
  , SocketType(..)
  , SSocketType(..)
  , openSocket
  , closeSocket
  , dial
  , listen
  , sendByteString
  ) where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import Foreign.C (CString)
import Foreign.C.Types (CInt)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Text.Encoding as Text

import qualified Libnng


-- TODO Address more protocols
data Address
  = Address'Inproc Text

addressAsCString
  :: Address
  -> ( CString -> IO a )
  -> IO a
addressAsCString address =
  ByteString.useAsCString
    ( Text.encodeUtf8 ( addressToText address ) )

addressToText
  :: Address
  -> Text
addressToText = \case
  Address'Inproc address -> "inproc://" <> address

newtype Socket ( ty :: SocketType )
  = Socket { unSocket :: Libnng.Socket }

data SocketType
  = SocketType'Rep
  | SocketType'Req

data SSocketType :: SocketType -> Type where
  SSocketType'Rep :: SSocketType 'SocketType'Rep
  SSocketType'Req :: SSocketType 'SocketType'Req


openSocket
  :: SSocketType ty
  -> IO ( Either Int ( Socket ty ) )
openSocket = \case
  SSocketType'Rep ->
    Libnng.rep0_open <&> \case
      Left err ->
        Left ( fromIntegral err )

      Right socket ->
        Right ( Socket socket )

  SSocketType'Req ->
    Libnng.req0_open <&> \case
      Left err ->
        Left ( fromIntegral err )

      Right socket ->
        Right ( Socket socket )

closeSocket
  :: Socket ty
  -> IO ( Either Int () )
closeSocket socket =
  Libnng.close ( unSocket socket ) <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right () ->
      Right ()

-- TODO dial flags
-- TODO dial type safety
dial
  :: Socket ty
  -> Address
  -> IO ( Either Int Libnng.Dialer )
dial socket address =
  doDial <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right dialer ->
      Right dialer

  where
    doDial :: IO ( Either CInt Libnng.Dialer )
    doDial =
      addressAsCString
        address
        ( \c_address ->
            Libnng.dial
              ( unSocket socket )
              c_address
              0
        )

-- TODO listen flags
-- TODO listen type safety
listen
  :: Socket ty
  -> Address
  -> IO ( Either Int Libnng.Listener )
listen socket address =
  doListen <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right listener ->
      Right listener

  where
    doListen :: IO ( Either CInt Libnng.Listener )
    doListen =
      addressAsCString
        address
        ( \c_address ->
            Libnng.listen
              ( unSocket socket )
              c_address
              0
        )

-- TODO sendByteString flags
-- TODO sendByteString type safety
sendByteString
  :: Socket ty
  -> ByteString
  -> IO ( Either Int () )
sendByteString socket bytes =
  doSend <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right () ->
      Right ()

  where
    doSend :: IO ( Either CInt () )
    doSend =
      ByteString.unsafeUseAsCStringLen
        bytes
        ( \( ptr, len ) ->
            Libnng.send
              ( unSocket socket )
              ptr
              ( fromIntegral len )
              0
        )
