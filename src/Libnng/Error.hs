module Libnng.Error
  ( Error(..)
  , cintToError
  ) where

import Control.Exception (Exception)
import Foreign.C (CInt)


data Error
  = Interrupted
  | OutOfMemory
  | InvalidArgument
  | ResourceBusy
  | TimedOut
  | ConnectionRefused
  | ObjectClosed
  | TryAgain
  | NotSupported
  | AddressInUse
  | IncorrectState
  | EntryNotFound
  | ProtocolError
  | DestinationUnreachable
  | AddressInvalid
  | PermissionDenied
  | MessageTooLarge
  | ConnectionAborted
  | ConnectionReset
  | OperationCanceled
  | OutOfFiles
  | OutOfSpace
  | ResourceAlreadyExists
  | ReadOnlyResource
  | WriteOnlyResource
  | CryptographicError
  | PeerCouldNotBeAuthenticated
  | OptionRequiresArgument
  | AmbiguousOption
  | IncorrectType
  deriving stock ( Eq, Show )
  deriving anyclass ( Exception )

cintToError
  :: CInt
  -> Error
cintToError = \case
  1  -> Interrupted
  2  -> OutOfMemory
  3  -> InvalidArgument
  4  -> ResourceBusy
  5  -> TimedOut
  6  -> ConnectionRefused
  7  -> ObjectClosed
  8  -> TryAgain
  9  -> NotSupported
  10 -> AddressInUse
  11 -> IncorrectState
  12 -> EntryNotFound
  13 -> ProtocolError
  14 -> DestinationUnreachable
  15 -> AddressInvalid
  16 -> PermissionDenied
  17 -> MessageTooLarge
  18 -> ConnectionAborted
  19 -> ConnectionReset
  20 -> OperationCanceled
  21 -> OutOfFiles
  22 -> OutOfSpace
  23 -> ResourceAlreadyExists
  24 -> ReadOnlyResource
  25 -> WriteOnlyResource
  26 -> CryptographicError
  27 -> PeerCouldNotBeAuthenticated
  28 -> OptionRequiresArgument
  29 -> AmbiguousOption
  30 -> IncorrectType
  n  -> error ( "cintToError: " ++ show n )
