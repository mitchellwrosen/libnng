{-# LANGUAGE UndecidableInstances #-}

module Nng
  ( Socket
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
  , openReqSocket
  , openRespondentSocket
  , openSubSocket
  , openSurveyorSocket
  , closeSocket
  , openDialer
  , openDialer_
  , closeDialer
  , listen
  , listen_
  , closeListener
  , sendByteString
  , recvByteString
    -- * Socket address
  , Address(..)
  , addressFromText
  , addressToText
  ) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Foreign
import Foreign.C
import GHC.Conc (threadWaitRead)
import GHC.TypeLits (TypeError)
import System.Posix.Types
import qualified Data.ByteString.Unsafe as ByteString
import qualified GHC.TypeLits as TypeError (ErrorMessage(..))

import Nng.Address
import Nng.Error
import qualified Libnng


newtype Socket ( ty :: SocketType )
  = Socket { unSocket :: Libnng.Socket }

data SocketType
  = SocketType'Bus
  | SocketType'Pair
  | SocketType'Pub
  | SocketType'Pull
  | SocketType'Push
  | SocketType'Rep
  | SocketType'Req
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
  SSocketType'Req        :: SSocketType 'SocketType'Req
  SSocketType'Respondent :: SSocketType 'SocketType'Respondent
  SSocketType'Sub        :: SSocketType 'SocketType'Sub
  SSocketType'Surveyor   :: SSocketType 'SocketType'Surveyor

type family CanSend (ty :: SocketType) :: Constraint where
  CanSend 'SocketType'Bus        = ()
  CanSend 'SocketType'Pair       = ()
  CanSend 'SocketType'Pub        = ()
  CanSend 'SocketType'Push       = ()
  CanSend 'SocketType'Rep        = ()
  CanSend 'SocketType'Req        = ()
  CanSend 'SocketType'Respondent = ()
  CanSend 'SocketType'Surveyor   = ()

  CanSend 'SocketType'Pull =
    TypeError ( 'TypeError.Text "You may not send on a pull socket" )
  CanSend 'SocketType'Sub =
    TypeError ( 'TypeError.Text "You may not send on a sub socket" )

type family CanReceive (ty :: SocketType) :: Constraint where
  CanReceive 'SocketType'Bus        = ()
  CanReceive 'SocketType'Pair       = ()
  CanReceive 'SocketType'Pull       = ()
  CanReceive 'SocketType'Rep        = ()
  CanReceive 'SocketType'Req        = ()
  CanReceive 'SocketType'Respondent = ()
  CanReceive 'SocketType'Sub        = ()
  CanReceive 'SocketType'Surveyor   = ()

  CanReceive 'SocketType'Pub =
    TypeError ( 'TypeError.Text "You may not receive on a pub socket" )
  CanReceive 'SocketType'Push =
    TypeError ( 'TypeError.Text "You may not receive on a push socket" )

openSocket
  :: SSocketType ty
  -> IO ( Either Error ( Socket ty ) )
openSocket = \case
  SSocketType'Bus        -> open Libnng.bus0_open
  SSocketType'Pair       -> open Libnng.pair1_open
  SSocketType'Pub        -> open Libnng.pub0_open
  SSocketType'Pull       -> open Libnng.pull0_open
  SSocketType'Push       -> open Libnng.push0_open
  SSocketType'Rep        -> open Libnng.rep0_open
  SSocketType'Req        -> open Libnng.req0_open
  SSocketType'Respondent -> open Libnng.respondent0_open
  SSocketType'Sub        -> open Libnng.sub0_open
  SSocketType'Surveyor   -> open Libnng.surveyor0_open

  where
    open
      :: IO ( Either CInt Libnng.Socket )
      -> IO ( Either Error ( Socket ty ) )
    open f =
      f <&> \case
        Left err ->
          Left ( cintToError err )

        Right socket ->
          Right ( Socket socket )

openBusSocket :: IO ( Either Error ( Socket 'SocketType'Bus ) )
openBusSocket =
  openSocket SSocketType'Bus

openPairSocket :: IO ( Either Error ( Socket 'SocketType'Pair ) )
openPairSocket =
  openSocket SSocketType'Pair

openPubSocket :: IO ( Either Error ( Socket 'SocketType'Pub ) )
openPubSocket =
  openSocket SSocketType'Pub

openPullSocket :: IO ( Either Error ( Socket 'SocketType'Pull ) )
openPullSocket =
  openSocket SSocketType'Pull

openPushSocket :: IO ( Either Error ( Socket 'SocketType'Push ) )
openPushSocket =
  openSocket SSocketType'Push

openRepSocket :: IO ( Either Error ( Socket 'SocketType'Rep ) )
openRepSocket =
  openSocket SSocketType'Rep

openReqSocket :: IO ( Either Error ( Socket 'SocketType'Req ) )
openReqSocket =
  openSocket SSocketType'Req

openRespondentSocket :: IO ( Either Error ( Socket 'SocketType'Respondent ) )
openRespondentSocket =
  openSocket SSocketType'Respondent

openSubSocket :: IO ( Either Error ( Socket 'SocketType'Sub ) )
openSubSocket =
  openSocket SSocketType'Sub

openSurveyorSocket :: IO ( Either Error ( Socket 'SocketType'Surveyor ) )
openSurveyorSocket =
  openSocket SSocketType'Surveyor

closeSocket
  :: Socket ty
  -> IO ( Either Error () )
closeSocket socket =
  errnoToError ( Libnng.close ( unSocket socket ) )

getRecvFd
  :: CanReceive ty
  => Socket ty
  -> IO ( Either Error Fd )
getRecvFd socket =
  coerce
    ( errnoToError
        ( Libnng.getopt_int
            ( unSocket socket )
            Libnng.oPT_RECVFD
        )
    )

getSendFd
  :: CanSend ty
  => Socket ty
  -> IO ( Either Error Fd )
getSendFd socket =
  coerce
    ( errnoToError
        ( Libnng.getopt_int
            ( unSocket socket )
            Libnng.oPT_SENDFD
        )
    )

-- TODO dial flags
-- TODO dial type safety
openDialer
  :: Socket ty
  -> Address
  -> IO ( Either Error Libnng.Dialer )
openDialer socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.dial
              ( unSocket socket )
              c_address
              0
        )
    )

openDialer_
  :: Socket ty
  -> Address
  -> IO ( Either Error () )
openDialer_ socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.dial_
              ( unSocket socket )
              c_address
              0
        )
    )

closeDialer
  :: Libnng.Dialer
  -> IO ( Either Error () )
closeDialer dialer =
  errnoToError ( Libnng.dialer_close dialer )

-- TODO listen flags
-- TODO listen type safety
listen
  :: Socket ty
  -> Address
  -> IO ( Either Error Libnng.Listener )
listen socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.listen
              ( unSocket socket )
              c_address
              0
        )
    )

listen_
  :: Socket ty
  -> Address
  -> IO ( Either Error () )
listen_ socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.listen_
              ( unSocket socket )
              c_address
              0
        )
    )

closeListener
  :: Libnng.Listener
  -> IO ( Either Error () )
closeListener listener =
  errnoToError ( Libnng.listener_close listener )

-- TODO sendByteString flags
sendByteString
  :: CanSend ty
  => Socket ty
  -> ByteString
  -> IO ( Either Error () )
sendByteString socket bytes =
  loop >>= \case
    Left Error'TryAgain ->
      getSendFd socket >>= \case
        Left err ->
          pure ( Left err )

        Right sendFd -> do
          threadWaitRead sendFd
          loop

    Left err ->
      pure ( Left err )

    Right () ->
      pure ( Right () )

  where
    loop :: IO ( Either Error () )
    loop =
      errnoToError
        ( ByteString.unsafeUseAsCStringLen
            bytes
            ( \( ptr, len ) ->
                Libnng.send_unsafe
                  ( unSocket socket )
                  ptr
                  ( fromIntegral len )
                  Libnng.fLAG_NONBLOCK
            )
        )

-- TODO recvByteString async exception safety
recvByteString
  :: CanReceive ty
  => Socket ty
  -> IO ( Either Error ByteString )
recvByteString socket =
  loop >>= \case
    Left Error'TryAgain ->
      getRecvFd socket >>= \case
        Left err ->
          pure ( Left err )

        Right recvFd -> do
          threadWaitRead recvFd
          loop

    Left err ->
      pure ( Left err )

    Right bytes ->
      pure ( Right bytes )

  where
    loop :: IO ( Either Error ByteString )
    loop =
      errnoToError
        ( alloca \ptrPtr ->
            alloca \lenPtr ->
              doRecv ptrPtr lenPtr >>= \case
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
        )

    doRecv
      :: Ptr ( Ptr Word8 )
      -> Ptr CSize
      -> IO ( Either CInt () )
    doRecv ptrPtr lenPtr =
      Libnng.recv_unsafe
        ( unSocket socket )
        ptrPtr
        lenPtr
        ( Libnng.fLAG_ALLOC .|. Libnng.fLAG_NONBLOCK )


--------------------------------------------------------------------------------
-- Misc. helpers
--------------------------------------------------------------------------------

errnoToError
  :: IO ( Either CInt a )
  -> IO ( Either Error a )
errnoToError =
  fmap
    ( \case
        Left err ->
          Left ( cintToError err )

        Right x ->
          Right x
    )
