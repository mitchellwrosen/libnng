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

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Foreign
import Foreign.C
import GHC.Conc (threadWaitRead)
import GHC.IO (unsafeUnmask)
import GHC.TypeLits (TypeError)
import System.Posix.Types
import qualified Data.ByteString.Unsafe as ByteString
import qualified GHC.TypeLits as TypeError (ErrorMessage(..))

import Nng.Address
import Nng.Error
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

type family SocketX ( ty :: SocketType ) :: Type where
  SocketX 'SocketType'Bus = ()
  SocketX 'SocketType'Pair = ()
  SocketX 'SocketType'Pub = ()
  SocketX 'SocketType'Pull = ()
  SocketX 'SocketType'Push = ()
  SocketX 'SocketType'Rep = ()
  SocketX 'SocketType'Req =
    ( ThreadId
    , TMVar
        ( ByteString
        , TMVar ( Either Error ByteString )
        )
    )
  SocketX 'SocketType'Respondent = ()
  SocketX 'SocketType'Sub = ()
  SocketX 'SocketType'Surveyor = ()

type family CanSend ( ty :: SocketType ) :: Constraint where
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

type family CanReceive ( ty :: SocketType ) :: Constraint where
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

openReqSocket :: IO ( Either Error ( Socket 'SocketType'Req ) )
openReqSocket =
  Libnng.req0_open >>= \case
    Left err ->
      pure ( Left ( cintToError err ) )

    Right socket -> do
      requestVar :: TMVar ( ByteString, TMVar ( Either Error ByteString ) ) <-
        newEmptyTMVarIO

      managerThread :: ThreadId <-
        forkIO do
          unsafeUnmask do
            forever do
              ( request, responseVar ) <-
                atomically ( readTMVar requestVar )

              sendByteString_ socket request >>= \case
                Left err ->
                  atomically ( putTMVar responseVar ( Left err ) )

                Right () ->
                  recvByteString_ socket >>= \case
                    Left err ->
                      atomically ( putTMVar responseVar ( Left err ) )

                    Right response ->
                      atomically ( putTMVar responseVar ( Right response ) )

      pure
        ( Right Socket
            { socketSocket = socket
            , socketType = SSocketType'Req
            , socketX = ( managerThread, requestVar )
            }
        )

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

-- TODO kill req socket manager thread
closeSocket
  :: Socket ty
  -> IO ( Either Error () )
closeSocket socket =
  errnoToError ( Libnng.close ( socketSocket socket ) )

getRecvFd
  :: Libnng.Socket
  -> IO ( Either Error Fd )
getRecvFd socket =
  coerce
    ( errnoToError
        ( Libnng.getopt_int
            socket
            Libnng.oPT_RECVFD
        )
    )

getSendFd
  :: Libnng.Socket
  -> IO ( Either Error Fd )
getSendFd socket =
  coerce
    ( errnoToError
        ( Libnng.getopt_int
            socket
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
              ( socketSocket socket )
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
              ( socketSocket socket )
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
openListener
  :: Socket ty
  -> Address
  -> IO ( Either Error Libnng.Listener )
openListener socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.listen
              ( socketSocket socket )
              c_address
              0
        )
    )

openListener_
  :: Socket ty
  -> Address
  -> IO ( Either Error () )
openListener_ socket address =
  errnoToError
    ( addressAsCString
        address
        ( \c_address ->
            Libnng.listen_
              ( socketSocket socket )
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
sendByteString socket =
  sendByteString_ ( socketSocket socket )

sendByteString_
  :: Libnng.Socket
  -> ByteString
  -> IO ( Either Error () )
sendByteString_ socket bytes =
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
                  socket
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
  recvByteString_ ( socketSocket socket )

recvByteString_
  :: Libnng.Socket
  -> IO ( Either Error ByteString )
recvByteString_ socket =
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
        socket
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
