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
  socket :: Nng.Socket 'Nng.SocketType'Req <-
    onLeftThrowIO ( Nng.openSocket Nng.SSocketType'Req )

  onLeftThrowIO ( Nng.dial_ socket address )

  putStrLn "Client: sending empty byte array"
  Nng.sendByteString socket ""

  response <- onLeftThrowIO ( Nng.recvByteString socket )
  print response

serverMain
  :: Nng.Address
  -> IO ()
serverMain address = do
  socket :: Nng.Socket 'Nng.SocketType'Rep <-
    onLeftThrowIO ( Nng.openSocket Nng.SSocketType'Rep )

  onLeftThrowIO ( Nng.listen_ socket address )

  threadDelay maxBound

onLeftThrowIO
  :: Exception e
  => IO ( Either e a )
  -> IO a
onLeftThrowIO action =
  action >>= either throwIO pure
