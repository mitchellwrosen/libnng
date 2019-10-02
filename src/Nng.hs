{-# LANGUAGE UndecidableInstances #-}

module Nng
  ( IsSocket(..)
  , ReqSocket
  , Socket
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
  , openRepSocket
  , openRespondentSocket
  , openSubSocket
  , openSurveyorSocket
  , openDialer
  , openDialer_
  , closeDialer
  , openListener
  , openListener_
  , closeListener
  , sendByteString
  , recvByteString
    -- * Socket address
  , Address(..)
  , addressFromText
  , addressToText
  ) where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (TypeError)
import qualified GHC.TypeLits as TypeError (ErrorMessage(..))

import Nng.Address
import Nng.Error
import Nng.Socket.Internal
import Nng.Socket.Req (ReqSocket)
import qualified Libnng


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
  | SocketType'Rep
  | SocketType'Respondent
  | SocketType'Sub
  | SocketType'Surveyor

data SSocketType :: SocketType -> Type where
  SSocketType'Bus        :: SSocketType 'SocketType'Bus
  SSocketType'Pair       :: SSocketType 'SocketType'Pair
  SSocketType'Pub        :: SSocketType 'SocketType'Pub
  SSocketType'Pull       :: SSocketType 'SocketType'Pull
  SSocketType'Push       :: SSocketType 'SocketType'Push
  SSocketType'Rep        :: SSocketType 'SocketType'Rep
  SSocketType'Respondent :: SSocketType 'SocketType'Respondent
  SSocketType'Sub        :: SSocketType 'SocketType'Sub
  SSocketType'Surveyor   :: SSocketType 'SocketType'Surveyor

type family SocketX ( ty :: SocketType ) :: Type where
  SocketX 'SocketType'Bus = ()
  SocketX 'SocketType'Pair = ()
  SocketX 'SocketType'Pub = ()
  SocketX 'SocketType'Pull = ()
  SocketX 'SocketType'Push = ()
  SocketX 'SocketType'Rep = ()
  SocketX 'SocketType'Respondent = ()
  SocketX 'SocketType'Sub = ()
  SocketX 'SocketType'Surveyor = ()

type family CanSend ( ty :: SocketType ) :: Constraint where
  CanSend 'SocketType'Bus        = ()
  CanSend 'SocketType'Pair       = ()
  CanSend 'SocketType'Pub        = ()
  CanSend 'SocketType'Push       = ()
  CanSend 'SocketType'Rep        = ()
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
  CanReceive 'SocketType'Rep        = ()
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

openRepSocket :: IO ( Either Error ( Socket 'SocketType'Rep ) )
openRepSocket =
  Libnng.rep0_open <&> \case
    Left err ->
      Left ( cintToError err )

    Right socket ->
      Right Socket
        { socketSocket = socket
        , socketType = SSocketType'Rep
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

-- TODO sendByteString flags
sendByteString
  :: CanSend ty
  => Socket ty
  -> ByteString
  -> IO ( Either Error () )
sendByteString socket =
  sendByteString_ ( socketSocket socket )

-- TODO recvByteString async exception safety
recvByteString
  :: CanReceive ty
  => Socket ty
  -> IO ( Either Error ByteString )
recvByteString socket =
  recvByteString_ ( socketSocket socket )
