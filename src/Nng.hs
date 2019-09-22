{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Nng
  ( Socket
  , SocketType(..)
  , SSocketType(..)
  , CanSend
  , Error(..)
  , openSocket
  , closeSocket
  , dial
  , listen
  , sendByteString
  , recvByteString
  ) where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import Foreign
import Foreign.C
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

type family CanSend (ty :: SocketType) :: Bool where
  CanSend 'SocketType'Rep = 'True
  CanSend 'SocketType'Req = 'True

type family CanReceive (ty :: SocketType) :: Bool where
  CanReceive 'SocketType'Rep = 'True
  CanReceive 'SocketType'Req = 'True

data Error
  = Error'Interrupted
  | Error'OutOfMemory
  | Error'InvalidArgument
  | Error'ResourceBusy
  | Error'TimedOut
  | Error'ConnectionRefused
  | Error'ObjectClosed
  | Error'TryAgain
  | Error'NotSupported
  | Error'AddressInUse
  | Error'IncorrectState
  | Error'EntryNotFound
  | Error'ProtocolError
  | Error'DestinationUnreachable
  | Error'AddressInvalid
  | Error'PermissionDenied
  | Error'MessageTooLarge
  | Error'ConnectionAborted
  | Error'ConnectionReset
  | Error'OperationCanceled
  | Error'OutOfFiles
  | Error'OutOfSpace
  | Error'ResourceAlreadyExists
  | Error'ReadOnlyResource
  | Error'WriteOnlyResource
  | Error'CryptographicError
  | Error'PeerCouldNotBeAuthenticated
  | Error'OptionRequiresArgument
  | Error'AmbiguousOption
  | Error'IncorrectType

cintToError
  :: CInt
  -> Error
cintToError = \case
  1  -> Error'Interrupted
  2  -> Error'OutOfMemory
  3  -> Error'InvalidArgument
  4  -> Error'ResourceBusy
  5  -> Error'TimedOut
  6  -> Error'ConnectionRefused
  7  -> Error'ObjectClosed
  8  -> Error'TryAgain
  9  -> Error'NotSupported
  10  -> Error'AddressInUse
  11 -> Error'IncorrectState
  12 -> Error'EntryNotFound
  13 -> Error'ProtocolError
  14 -> Error'DestinationUnreachable
  15 -> Error'AddressInvalid
  16 -> Error'PermissionDenied
  17 -> Error'MessageTooLarge
  18 -> Error'ConnectionAborted
  19 -> Error'ConnectionReset
  20 -> Error'OperationCanceled
  21 -> Error'OutOfFiles
  22 -> Error'OutOfSpace
  23 -> Error'ResourceAlreadyExists
  24 -> Error'ReadOnlyResource
  25 -> Error'WriteOnlyResource
  26 -> Error'CryptographicError
  27 -> Error'PeerCouldNotBeAuthenticated
  28 -> Error'OptionRequiresArgument
  29 -> Error'AmbiguousOption
  30 -> Error'IncorrectType
  n  -> error ( "cintToError: " ++ show n )

openSocket
  :: SSocketType ty
  -> IO ( Either Error ( Socket ty ) )
openSocket = \case
  SSocketType'Rep ->
    Libnng.rep0_open <&> \case
      Left err ->
        Left ( cintToError err )

      Right socket ->
        Right ( Socket socket )

  SSocketType'Req ->
    Libnng.req0_open <&> \case
      Left err ->
        Left ( cintToError err )

      Right socket ->
        Right ( Socket socket )

closeSocket
  :: Socket ty
  -> IO ( Either Error () )
closeSocket socket =
  Libnng.close ( unSocket socket ) <&> \case
    Left err ->
      Left ( cintToError err )

    Right () ->
      Right ()

-- TODO dial flags
-- TODO dial type safety
dial
  :: Socket ty
  -> Address
  -> IO ( Either Error Libnng.Dialer )
dial socket address =
  doDial <&> \case
    Left err ->
      Left ( cintToError err )

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
  -> IO ( Either Error Libnng.Listener )
listen socket address =
  doListen <&> \case
    Left err ->
      Left ( cintToError err )

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
sendByteString
  :: ( CanSend ty ~ 'True )
  => Socket ty
  -> ByteString
  -> IO ( Either Error () )
sendByteString socket bytes =
  doSend <&> \case
    Left err ->
      Left ( cintToError err )

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

-- | TODO recvByteString async exception safety
recvByteString
  :: ( CanReceive ty ~ 'True )
  => Socket ty
  -> IO ( Either Error ByteString )
recvByteString socket =
  doRecv <&> \case
    Left err ->
      Left ( cintToError err )

    Right message ->
      Right message

  where
    doRecv :: IO ( Either CInt ByteString )
    doRecv =
      alloca \ptrPtr ->
        alloca \lenPtr ->
          doRecv2 ptrPtr lenPtr >>= \case
            Left err ->
              pure ( Left err )

            Right () -> do
              ptr :: Ptr Word8 <-
                peek ptrPtr

              len :: CSize <-
                peek lenPtr

              Right <$>
                ByteString.unsafePackCStringFinalizer
                  ptr
                  ( fromIntegral len )
                  ( Libnng.free ptr len )

    doRecv2
      :: Ptr ( Ptr Word8 )
      -> Ptr CSize
      -> IO ( Either CInt () )
    doRecv2 ptrPtr lenPtr =
      Libnng.recv
        ( unSocket socket )
        ptrPtr
        lenPtr
        1 -- NNG_FLAG_ALLOC
