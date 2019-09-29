{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import Control.Exception (Exception, throwIO)
import Control.Concurrent -- (threadDelay)
import System.Environment
import System.Exit
import System.IO
import qualified Data.Text as Text

import qualified Nng

main :: IO ()
main =
  getArgs >>= \case
    [ "client", Nng.addressFromText . Text.pack -> Just address ] ->
      clientMain address

    [ "server", Nng.addressFromText . Text.pack -> Just address ] ->
      serverMain address

    _ -> do
      hPutStrLn stderr "Usage: (client | server) <address>"
      exitFailure

clientMain
  :: Nng.Address
  -> IO ()
clientMain address = do
  putStrLn "Opening socket"
  socket :: Nng.Socket 'Nng.SocketType'Req <-
    onLeftThrowIO ( Nng.openSocket Nng.SSocketType'Req )

  putStrLn ( "Dialing " ++ show address )
  onLeftThrowIO ( Nng.dial_ socket address )

  putStrLn "Sending byte array"
  onLeftThrowIO ( Nng.sendByteString socket "hello" )

  putStrLn "Receiving byte array"
  result <- onLeftThrowIO ( Nng.recvByteString socket )

  putStrLn ( "Recevied: " ++ show result )

serverMain
  :: Nng.Address
  -> IO ()
serverMain address = do
  putStrLn "Opening socket"
  socket :: Nng.Socket 'Nng.SocketType'Rep <-
    onLeftThrowIO ( Nng.openSocket Nng.SSocketType'Rep )

  putStrLn ( "Listening on " ++ show address )
  onLeftThrowIO ( Nng.listen_ socket address )

  let
    loop :: IO ()
    loop = do
      putStrLn "Receiving byte array"
      _ <- Nng.recvByteString socket

      putStrLn "Sleeping for 1s"
      threadDelay ( 1*1000*1000 )

      putStrLn "Sending byte array"
      onLeftThrowIO ( Nng.sendByteString socket "hello" )

      loop

  loop

onLeftThrowIO
  :: Exception e
  => IO ( Either e a )
  -> IO a
onLeftThrowIO action =
  action >>= either throwIO pure
