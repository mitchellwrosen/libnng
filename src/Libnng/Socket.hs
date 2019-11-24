module Libnng.Socket where

import Foreign
import Foreign.C

import Libnng.Error
import Libnng.Types
import Libnng.Utils


bus0_open :: IO ( Either Error Socket )
bus0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_bus0_open socketPtr )

foreign import ccall unsafe "static nng_bus0_open"
  nng_bus0_open
    :: Ptr Socket
    -> IO CInt

close
  :: Socket
  -> IO ( Either Error () )
close socket =
  errnoToEither ( nng_close socket )

foreign import ccall unsafe "static nng_close"
  nng_close
    :: Socket
    -> IO CInt

dial
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either Error Dialer )
dial socket url flags =
  alloca \dialerPtr ->
    onZero
      ( peek dialerPtr )
      ( nng_dial socket url dialerPtr flags )

dial_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either Error () )
dial_ socket url flags =
  errnoToEither ( nng_dial socket url nullPtr flags )

foreign import ccall safe "static nng_dial"
  nng_dial
    :: Socket
    -> CString
    -> Ptr Dialer
    -> CInt
    -> IO CInt

dial_unsafe
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either Error Dialer )
dial_unsafe socket url flags =
  alloca \dialerPtr ->
    onZero
      ( peek dialerPtr )
      ( nng_dial_unsafe socket url dialerPtr flags )

dial_unsafe_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either Error () )
dial_unsafe_ socket url flags =
  errnoToEither ( nng_dial_unsafe socket url nullPtr flags )

foreign import ccall unsafe "static nng_dial"
  nng_dial_unsafe
    :: Socket
    -> CString
    -> Ptr Dialer
    -> CInt
    -> IO CInt

dialer_close
  :: Dialer
  -> IO ( Either Error () )
dialer_close dialer =
  errnoToEither ( nng_dialer_close dialer )

foreign import ccall unsafe "static nng_dialer_close"
  nng_dialer_close
    :: Dialer
    -> IO CInt

dialer_create
  :: Socket
  -> CString
  -> IO ( Either Error Dialer )
dialer_create socket url =
  alloca \dialerPtr ->
    onZero
      ( peek dialerPtr )
      ( nng_dialer_create dialerPtr socket url )

foreign import ccall unsafe "static nng_dialer_create"
  nng_dialer_create
    :: Ptr Dialer
    -> Socket
    -> CString
    -> IO CInt

dialer_setopt_bool
  :: Dialer
  -> CString
  -> CBool
  -> IO ( Either Error () )
dialer_setopt_bool dialer opt val =
  errnoToEither ( nng_dialer_setopt_bool dialer opt val )

foreign import ccall unsafe "static nng_dialer_setopt_bool"
  nng_dialer_setopt_bool
    :: Dialer
    -> CString
    -> CBool
    -> IO CInt

dialer_setopt_int
  :: Dialer
  -> CString
  -> CInt
  -> IO ( Either Error () )
dialer_setopt_int dialer opt val =
  errnoToEither ( nng_dialer_setopt_int dialer opt val )

foreign import ccall unsafe "static nng_dialer_setopt_int"
  nng_dialer_setopt_int
    :: Dialer
    -> CString
    -> CInt
    -> IO CInt

dialer_start
  :: Dialer
  -> CInt
  -> IO ( Either Error () )
dialer_start dialer flags =
  errnoToEither ( nng_dialer_start dialer flags )

foreign import ccall safe "static nng_dialer_start"
  nng_dialer_start
    :: Dialer
    -> CInt
    -> IO CInt

dialer_start_unsafe
  :: Dialer
  -> CInt
  -> IO ( Either Error () )
dialer_start_unsafe dialer flags =
  errnoToEither ( nng_dialer_start_unsafe dialer flags )

foreign import ccall unsafe "static nng_dialer_start"
  nng_dialer_start_unsafe
    :: Dialer
    -> CInt
    -> IO CInt

getopt_int
  :: Socket
  -> CString
  -> IO ( Either Error CInt )
getopt_int socket opt =
  alloca \valPtr ->
    onZero
      ( peek valPtr )
      ( nng_getopt_int socket opt valPtr )

foreign import ccall unsafe "static nng_getopt_int"
  nng_getopt_int
    :: Socket
    -> CString
    -> Ptr CInt
    -> IO CInt

listen
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either Error Listener )
listen socket url flags =
  alloca \listenerPtr ->
    onZero
      ( peek listenerPtr )
      ( nng_listen socket url listenerPtr flags )

foreign import ccall unsafe "static nng_listen"
  nng_listen
    :: Socket
    -> CString
    -> Ptr Listener
    -> CInt
    -> IO CInt

listen_
  :: Socket
  -> CString
  -> CInt
  -> IO ( Either Error () )
listen_ socket url flags =
  errnoToEither ( nng_listen socket url nullPtr flags )

listener_close
  :: Listener
  -> IO ( Either Error () )
listener_close listener =
  errnoToEither ( nng_listener_close listener )

foreign import ccall unsafe "static nng_listener_close"
  nng_listener_close
    :: Listener
    -> IO CInt

listener_create
  :: Socket
  -> CString
  -> IO ( Either Error Listener )
listener_create socket url =
  alloca \listenerPtr ->
    onZero
      ( peek listenerPtr )
      ( nng_listener_create listenerPtr socket url )

foreign import ccall unsafe "static nng_listener_create"
  nng_listener_create
    :: Ptr Listener
    -> Socket
    -> CString
    -> IO CInt

listener_start
  :: Listener
  -> IO ( Either Error () )
listener_start listener =
  errnoToEither ( nng_listener_start listener 0 )

foreign import ccall unsafe "static nng_listener_start"
  nng_listener_start
    :: Listener
    -> CInt
    -> IO CInt

pair0_open :: IO ( Either Error Socket )
pair0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_pair0_open socketPtr )

foreign import ccall unsafe "static nng_pair0_open"
  nng_pair0_open
    :: Ptr Socket
    -> IO CInt

pair1_open :: IO ( Either Error Socket )
pair1_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_pair1_open socketPtr )

foreign import ccall unsafe "static nng_pair1_open"
  nng_pair1_open
    :: Ptr Socket
    -> IO CInt

pub0_open :: IO ( Either Error Socket )
pub0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_pub0_open socketPtr )

foreign import ccall unsafe "static nng_pub0_open"
  nng_pub0_open
    :: Ptr Socket
    -> IO CInt

pull0_open :: IO ( Either Error Socket )
pull0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_pull0_open socketPtr )

foreign import ccall unsafe "static nng_pull0_open"
  nng_pull0_open
    :: Ptr Socket
    -> IO CInt

push0_open :: IO ( Either Error Socket )
push0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_push0_open socketPtr )

foreign import ccall unsafe "static nng_push0_open"
  nng_push0_open
    :: Ptr Socket
    -> IO CInt

recv
  :: Socket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either Error () )
recv socket data_ size flags =
  errnoToEither ( nng_recv socket data_ size flags )

foreign import ccall safe "static nng_recv"
  nng_recv
    :: Socket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

recv_unsafe
  :: Socket
  -> Ptr a
  -> Ptr CSize
  -> CInt
  -> IO ( Either Error () )
recv_unsafe socket data_ size flags =
  errnoToEither ( nng_recv_unsafe socket data_ size flags )

foreign import ccall unsafe "static nng_recv"
  nng_recv_unsafe
    :: Socket
    -> Ptr a
    -> Ptr CSize
    -> CInt
    -> IO CInt

foreign import ccall safe "static nng_recvmsg"
  nng_recvmsg
    :: Socket
    -> Ptr Msg
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_recvmsg"
  nng_recvmsg_unsafe
    :: Socket
    -> Ptr Msg
    -> CInt
    -> IO CInt

foreign import ccall safe "static nng_sendmsg"
  nng_sendmsg
    :: Socket
    -> Msg
    -> CInt
    -> IO CInt

foreign import ccall unsafe "static nng_sendmsg"
  nng_sendmsg_unsafe
    :: Socket
    -> Msg
    -> CInt
    -> IO CInt

rep0_open :: IO ( Either Error Socket )
rep0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_rep0_open socketPtr )

foreign import ccall unsafe "static nng_rep0_open"
  nng_rep0_open
    :: Ptr Socket
    -> IO CInt

req0_open :: IO ( Either Error Socket )
req0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_req0_open socketPtr )

foreign import ccall unsafe "static nng_req0_open"
  nng_req0_open
    :: Ptr Socket
    -> IO CInt

respondent0_open :: IO ( Either Error Socket )
respondent0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_respondent0_open socketPtr )

foreign import ccall unsafe "static nng_respondent0_open"
  nng_respondent0_open
    :: Ptr Socket
    -> IO CInt

send
  :: Socket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either Error () )
send socket data_ size flags =
  errnoToEither ( nng_send socket data_ size flags )

foreign import ccall safe "static nng_send"
  nng_send
    :: Socket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

send_unsafe
  :: Socket
  -> Ptr a
  -> CSize
  -> CInt
  -> IO ( Either Error () )
send_unsafe socket data_ size flags =
  errnoToEither ( nng_send_unsafe socket data_ size flags )

foreign import ccall unsafe "static nng_send"
  nng_send_unsafe
    :: Socket
    -> Ptr a
    -> CSize
    -> CInt
    -> IO CInt

sub0_open :: IO ( Either Error Socket )
sub0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_sub0_open socketPtr )

foreign import ccall unsafe "static nng_sub0_open"
  nng_sub0_open
    :: Ptr Socket
    -> IO CInt

surveyor0_open :: IO ( Either Error Socket )
surveyor0_open =
  alloca \socketPtr ->
    onZero
      ( peek socketPtr )
      ( nng_surveyor0_open socketPtr )

foreign import ccall unsafe "static nng_surveyor0_open"
  nng_surveyor0_open
    :: Ptr Socket
    -> IO CInt
