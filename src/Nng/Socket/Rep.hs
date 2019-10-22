module Nng.Socket.Rep
  ( RepSocket
  , open
  , recv
  , send
  ) where

import Nng.Error
import Nng.Prelude
import Nng.Socket.Internal
import qualified Libnng


newtype RepSocket
  = RepSocket
  { repSocketSocket :: Libnng.Socket
  }

instance IsSocket RepSocket where
  close         = repSocketSocket >>> internalClose
  openDialer    = repSocketSocket >>> internalOpenDialer
  openDialer_   = repSocketSocket >>> internalOpenDialer_
  openListener  = repSocketSocket >>> internalOpenListener
  openListener_ = repSocketSocket >>> internalOpenListener_


open :: IO ( Either Error RepSocket )
open =
  Libnng.rep0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right RepSocket
        { repSocketSocket = socket
        }

recv
  :: RepSocket
  -> IO ( Either Error ByteString )
recv socket =
  internalRecvByteString ( repSocketSocket socket )

send
  :: RepSocket
  -> ByteString
  -> IO ( Either Error () )
send socket =
  internalSendByteString ( repSocketSocket socket )
