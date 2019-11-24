{-# LANGUAGE UndecidableInstances #-}

module Nng
  ( RepSocket
  , ReqSocket
  , Socket(..)
  , SocketType(..)
  , SSocketType(..)
  , CanSend
  , CanReceive
  , Error(..)
  , openBusSocket
  , openPairSocket
  , openPubSocket
  , openPullSocket
  , openPushSocket
  , openRespondentSocket
  , openSubSocket
  , openSurveyorSocket
  , openDialer
  , openDialer_
  , Libnng.dialer_close
  , openListener
  , openListener_
  , Libnng.listener_close
    -- * Socket address
  , Address(..)
  , addressFromText
  , addressToText
  ) where

import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (TypeError)
import qualified GHC.TypeLits as TypeError (ErrorMessage(..))

import Libnng (Error(..))
import Nng.Address
import Nng.Prelude
import Nng.Socket.Rep (RepSocket)
import Nng.Socket.Req (ReqSocket)
import qualified Libnng
import qualified Nng.Socket.Internal


data Socket ( ty :: SocketType )
  = Socket
  { socketSocket :: Libnng.Socket
  , socketType :: SSocketType ty
  , socketX :: SocketX ty
  }

data SocketType
  = SocketType'Bus
  | SocketType'Pair
  | SocketType'Pub
  | SocketType'Pull
  | SocketType'Push
  | SocketType'Respondent
  | SocketType'Sub
  | SocketType'Surveyor

data SSocketType :: SocketType -> Type where
  SSocketType'Bus        :: SSocketType 'SocketType'Bus
  SSocketType'Pair       :: SSocketType 'SocketType'Pair
  SSocketType'Pub        :: SSocketType 'SocketType'Pub
  SSocketType'Pull       :: SSocketType 'SocketType'Pull
  SSocketType'Push       :: SSocketType 'SocketType'Push
  SSocketType'Respondent :: SSocketType 'SocketType'Respondent
  SSocketType'Sub        :: SSocketType 'SocketType'Sub
  SSocketType'Surveyor   :: SSocketType 'SocketType'Surveyor

type family SocketX ( ty :: SocketType ) :: Type where
  SocketX 'SocketType'Bus = ()
  SocketX 'SocketType'Pair = ()
  SocketX 'SocketType'Pub = ()
  SocketX 'SocketType'Pull = ()
  SocketX 'SocketType'Push = ()
  SocketX 'SocketType'Respondent = ()
  SocketX 'SocketType'Sub = ()
  SocketX 'SocketType'Surveyor = ()

type family CanSend ( ty :: SocketType ) :: Constraint where
  CanSend 'SocketType'Bus        = ()
  CanSend 'SocketType'Pair       = ()
  CanSend 'SocketType'Pub        = ()
  CanSend 'SocketType'Push       = ()
  CanSend 'SocketType'Respondent = ()
  CanSend 'SocketType'Surveyor   = ()

  CanSend 'SocketType'Pull =
    TypeError ( 'TypeError.Text "You may not send on a pull socket" )
  CanSend 'SocketType'Sub =
    TypeError ( 'TypeError.Text "You may not send on a sub socket" )

type family CanReceive ( ty :: SocketType ) :: Constraint where
  CanReceive 'SocketType'Bus        = ()
  CanReceive 'SocketType'Pair       = ()
  CanReceive 'SocketType'Pull       = ()
  CanReceive 'SocketType'Respondent = ()
  CanReceive 'SocketType'Sub        = ()
  CanReceive 'SocketType'Surveyor   = ()

  CanReceive 'SocketType'Pub =
    TypeError ( 'TypeError.Text "You may not receive on a pub socket" )
  CanReceive 'SocketType'Push =
    TypeError ( 'TypeError.Text "You may not receive on a push socket" )


openBusSocket :: IO ( Either Error ( Socket 'SocketType'Bus ) )
openBusSocket =
  Libnng.bus0_open <&> \case
    Left err ->
      Left err

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Bus
        , socketX = ()
        }

openPairSocket :: IO ( Either Error ( Socket 'SocketType'Pair ) )
openPairSocket =
  ( fmap . fmap )
    ( \socket ->
        Socket
          { socketSocket = socket
          , socketType = SSocketType'Pair
          , socketX = ()
          }
    )
    Libnng.pair1_open

openPubSocket :: IO ( Either Error ( Socket 'SocketType'Pub ) )
openPubSocket =
  ( fmap . fmap )
    ( \socket ->
        Socket
          { socketSocket = socket
          , socketType = SSocketType'Pub
          , socketX = ()
          }
    )
    Libnng.pub0_open

openPullSocket :: IO ( Either Error ( Socket 'SocketType'Pull ) )
openPullSocket =
  ( fmap . fmap )
    ( \socket ->
        Socket
          { socketSocket = socket
          , socketType = SSocketType'Pull
          , socketX = ()
          }
    )
    Libnng.pull0_open

openPushSocket :: IO ( Either Error ( Socket 'SocketType'Push ) )
openPushSocket =
  ( fmap . fmap )
    ( \socket ->
        Socket
          { socketSocket = socket
          , socketType = SSocketType'Push
          , socketX = ()
          }
    )
    Libnng.push0_open

openRespondentSocket :: IO ( Either Error ( Socket 'SocketType'Respondent ) )
openRespondentSocket =
  ( fmap . fmap )
    ( \socket ->
        Socket
          { socketSocket = socket
          , socketType = SSocketType'Respondent
          , socketX = ()
          }
    )
    Libnng.respondent0_open

openSubSocket :: IO ( Either Error ( Socket 'SocketType'Sub ) )
openSubSocket =
  ( fmap . fmap )
    ( \socket ->
        Socket
          { socketSocket = socket
          , socketType = SSocketType'Sub
          , socketX = ()
          }
    )
    Libnng.sub0_open

openSurveyorSocket :: IO ( Either Error ( Socket 'SocketType'Surveyor ) )
openSurveyorSocket =
  ( fmap . fmap )
    ( \socket ->
        Socket
          { socketSocket = socket
          , socketType = SSocketType'Surveyor
          , socketX = ()
          }
    )
    Libnng.surveyor0_open

openDialer
  :: Socket ty
  -> Address
  -> IO ( Either Error Libnng.Dialer )
openDialer =
  socketSocket >>> Nng.Socket.Internal.internalOpenDialer

openDialer_
  :: Socket ty
  -> Address
  -> IO ( Either Error () )
openDialer_ =
  socketSocket >>> Nng.Socket.Internal.internalOpenDialer_

openListener
  :: Socket ty
  -> Address
  -> IO ( Either Error Libnng.Listener )
openListener =
  socketSocket >>> Nng.Socket.Internal.internalOpenListener

openListener_
  :: Socket ty
  -> Address
  -> IO ( Either Error () )
openListener_ =
  socketSocket >>> Nng.Socket.Internal.internalOpenListener_
