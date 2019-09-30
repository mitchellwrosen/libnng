module Libnng.Types where

import Data.Word (Word32)
import Foreign hiding (free)


newtype Aio
  = Aio ( Ptr () )
  deriving newtype ( Storable )

newtype Ctx
  = Ctx { ctx_id :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype Dialer
  = Dialer { dialer_id :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype Listener
  = Listener { listener_id :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )

newtype Socket
  = Socket { socket_id :: Word32 }
  deriving stock ( Eq, Show )
  deriving newtype ( Storable )
