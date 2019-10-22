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
  , Nng.Socket.Internal.closeDialer
  , openListener
  , openListener_
  , Nng.Socket.Internal.closeListener
    -- * Socket address
  , Address(..)
  , addressFromText
  , addressToText
  ) where

import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (TypeError)
import qualified GHC.TypeLits as TypeError (ErrorMessage(..))

import Nng.Address
import Nng.Error
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
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Bus
        , socketX = ()
        }

openPairSocket :: IO ( Either Error ( Socket 'SocketType'Pair ) )
openPairSocket =
  Libnng.pair1_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Pair
        , socketX = ()
        }

openPubSocket :: IO ( Either Error ( Socket 'SocketType'Pub ) )
openPubSocket =
  Libnng.pub0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Pub
        , socketX = ()
        }

openPullSocket :: IO ( Either Error ( Socket 'SocketType'Pull ) )
openPullSocket =
  Libnng.pull0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Pull
        , socketX = ()
        }

openPushSocket :: IO ( Either Error ( Socket 'SocketType'Push ) )
openPushSocket =
  Libnng.push0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Push
        , socketX = ()
        }

openRespondentSocket :: IO ( Either Error ( Socket 'SocketType'Respondent ) )
openRespondentSocket =
  Libnng.respondent0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Respondent
        , socketX = ()
        }

openSubSocket :: IO ( Either Error ( Socket 'SocketType'Sub ) )
openSubSocket =
  Libnng.sub0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Sub
        , socketX = ()
        }

openSurveyorSocket :: IO ( Either Error ( Socket 'SocketType'Surveyor ) )
openSurveyorSocket =
  Libnng.surveyor0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Surveyor
        , socketX = ()
        }

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
