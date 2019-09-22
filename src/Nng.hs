{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nng
  ( NngSocket
  , NngSocketType(..)
  , SNngSocketType(..)
  , openNngSocket
  , closeNngSocket
  , nngDial
  , nngListen
  , nngSendByteString
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

import Libnng hiding (NngSocket)
import qualified Libnng


-- TODO NngAddress more protocols
data NngAddress
  = NngAddress'Inproc Text

nngAddressAsCString
  :: NngAddress
  -> ( CString -> IO a )
  -> IO a
nngAddressAsCString address =
  ByteString.useAsCString
    ( Text.encodeUtf8 ( nngAddressToText address ) )

nngAddressToText
  :: NngAddress
  -> Text
nngAddressToText = \case
  NngAddress'Inproc address -> "inproc://" <> address

newtype NngSocket ( ty :: NngSocketType )
  = NngSocket { unNngSocket :: Libnng.NngSocket }

data NngSocketType
  = NngSocketType'Rep
  | NngSocketType'Req

data SNngSocketType :: NngSocketType -> Type where
  SNngSocketType'Rep :: SNngSocketType 'NngSocketType'Rep
  SNngSocketType'Req :: SNngSocketType 'NngSocketType'Req


openNngSocket
  :: SNngSocketType ty
  -> IO ( Either Int ( NngSocket ty ) )
openNngSocket = \case
  SNngSocketType'Rep ->
    nng_rep0_open <&> \case
      Left err ->
        Left ( fromIntegral err )

      Right socket ->
        Right ( NngSocket socket )

  SNngSocketType'Req ->
    nng_req0_open <&> \case
      Left err ->
        Left ( fromIntegral err )

      Right socket ->
        Right ( NngSocket socket )

closeNngSocket
  :: NngSocket ty
  -> IO ( Either Int () )
closeNngSocket socket =
  nng_close ( unNngSocket socket ) <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right () ->
      Right ()

-- TODO nngDial flags
-- TODO nngDial type safety
nngDial
  :: NngSocket ty
  -> NngAddress
  -> IO ( Either Int NngDialer )
nngDial socket address =
  dial <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right dialer ->
      Right dialer

  where
    dial :: IO ( Either CInt NngDialer )
    dial =
      nngAddressAsCString
        address
        ( \c_address ->
            nng_dial
              ( unNngSocket socket )
              c_address
              0
        )

-- TODO nngListen flags
-- TODO nngListen type safety
nngListen
  :: NngSocket ty
  -> NngAddress
  -> IO ( Either Int NngListener )
nngListen socket address =
  listen <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right listener ->
      Right listener

  where
    listen :: IO ( Either CInt NngListener )
    listen =
      nngAddressAsCString
        address
        ( \c_address ->
            nng_listen
              ( unNngSocket socket )
              c_address
              0
        )

-- TODO nngSendByteString flags
-- TODO nngSendByteString type safety
nngSendByteString
  :: NngSocket ty
  -> ByteString
  -> IO ( Either Int () )
nngSendByteString socket bytes =
  send <&> \case
    Left err ->
      Left ( fromIntegral err )

    Right () ->
      Right ()

  where
    send :: IO ( Either CInt () )
    send =
      ByteString.unsafeUseAsCStringLen
        bytes
        ( \( ptr, len ) ->
            nng_send
              ( unNngSocket socket )
              ptr
              ( fromIntegral len )
              0
        )
