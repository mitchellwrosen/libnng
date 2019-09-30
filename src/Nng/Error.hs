module Nng.Error
  ( Error(..)
  , cintToError
  ) where

import Control.Exception (Exception)
import Foreign.C.Types (CInt)

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
  deriving stock ( Eq, Show )
  deriving anyclass ( Exception )

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
  10 -> Error'AddressInUse
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

