{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Set (Set)
import Foreign.C.String
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Set as Set

import Libnng

passed :: IORef ( Set String )
passed =
  unsafePerformIO ( newIORef Set.empty )
{-# NOINLINE passed #-}

main :: IO ()
main = do
  do
    names <-
      Set.fromList . lines <$>
        ( readFile ".testcache" <|> pure "" )
    writeIORef passed names

  test_close
  test_dial
  test_dialer_close
  test_listen
  test_rep0_open
  test_req0_open
  test_strerror

  do
    names <- readIORef passed
    writeFile ".testcache" ( unlines ( Set.toList names ) )

test_close :: IO ()
test_close = do
  test "close returns Right on open socket" do
    socket <- shouldReturnRight req0_open
    close socket `shouldReturn` Right ()

  test "close returns Left 7 on closed socket" do
    socket <- shouldReturnRight req0_open
    close socket `shouldReturn` Right ()
    close socket `shouldReturn` Left 7

test_dial :: IO ()
test_dial = do
  test "dial returns Left 3 on bogus address" do
    with_req0 \socket ->
      withCString "foo" \address ->
        dial socket address 0 `shouldReturn` Left 3

  test "dial returns Left 6 on endpoint that's not listening" do
    with_req0 \socket ->
      withCString "inproc://foo" \address ->
        dial socket address 0 `shouldReturn` Left 6

  -- TODO dial protocol error

  test "dial returns Right" do
    with_req0 \req ->
      with_rep0 \rep ->
        withCString "inproc://foo" \address -> do
          shouldReturnRight ( listen_ rep address 0 )
          shouldReturnRight ( dial_ req address 0 )

test_dialer_close :: IO ()
test_dialer_close =  do
  test "dialer_close returns Right on open dialer" do
    with_req0 \req ->
      with_rep0 \rep ->
        withCString "inproc://foo" \address -> do
          shouldReturnRight ( listen_ rep address 0 )
          dialer <- shouldReturnRight ( dial req address 0 )
          dialer_close dialer `shouldReturn` Right ()

test_listen :: IO ()
test_listen = do
  test "listen returns Left 3 on bogus address" do
    with_rep0 \socket ->
      withCString "foo" \address ->
        listen socket address 0 `shouldReturn` Left 3

  test "listen returns Right" do
    with_rep0 \socket ->
      withCString "inproc://foo" \address ->
        shouldReturnRight ( listen_ socket address 0 )

  -- TODO listen protocol error

test_rep0_open :: IO ()
test_rep0_open = do
  test "rep0_open returns Right" do
    socket <- shouldReturnRight rep0_open
    close socket `shouldReturn` Right ()

test_req0_open :: IO ()
test_req0_open = do
  test "req0_open returns Right" do
    socket <- shouldReturnRight req0_open
    close socket `shouldReturn` Right ()

test_strerror :: IO ()
test_strerror = do
  traverse_
    ( \(n, s) ->
        test ( "strerror " ++ show n ) do
          s' <- peekCString ( strerror n )
          s' `shouldBe` s
    )
    [ ( 0,  "Hunky dory"                      )
    , ( 1,  "Interrupted"                     )
    , ( 2,  "Out of memory"                   )
    , ( 3,  "Invalid argument"                )
    , ( 4,  "Resource busy"                   )
    , ( 5,  "Timed out"                       )
    , ( 6,  "Connection refused"              )
    , ( 7,  "Object closed"                   )
    , ( 8,  "Try again"                       )
    , ( 9,  "Not supported"                   )
    , ( 10, "Address in use"                  )
    , ( 11, "Incorrect state"                 )
    , ( 12, "Entry not found"                 )
    , ( 13, "Protocol error"                  )
    , ( 14, "Destination unreachable"         )
    , ( 15, "Address invalid"                 )
    , ( 16, "Permission denied"               )
    , ( 17, "Message too large"               )
    , ( 18, "Connection aborted"              )
    , ( 19, "Connection reset"                )
    , ( 20, "Operation canceled"              )
    , ( 21, "Out of files"                    )
    , ( 22, "Out of space"                    )
    , ( 23, "Resource already exists"         )
    , ( 24, "Read only resource"              )
    , ( 25, "Write only resource"             )
    , ( 26, "Cryptographic error"             )
    , ( 27, "Peer could not be authenticated" )
    , ( 28, "Option requires argument"        )
    , ( 29, "Ambiguous option"                )
    , ( 30, "Incorrect type"                  )
    , ( 31, "Unknown error #31"               ) -- last one!
    ]

  test "version is 1.1.1" do
    peekCString version `shouldReturn` "1.1.1"

with_rep0
  :: ( Socket -> IO a )
  -> IO a
with_rep0 action =
  bracket
    rep0_open
    ( either
        ( const ( pure () ) )
        ( void . close )
    )
    ( either
        ( \n -> throwIO ( userError ( "with_rep0: " ++ show n ) ) )
        action
    )

with_req0
  :: ( Socket -> IO a )
  -> IO a
with_req0 action =
  bracket
    req0_open
    ( either
        ( const ( pure () ) )
        ( void . close )
    )
    ( either
        ( \n -> throwIO ( userError ( "with_req0: " ++ show n ) ) )
        action
    )

test
  :: String
  -> IO ()
  -> IO ()
test name action = do
  names <- readIORef passed

  if Set.member name names
    then
      putStrLn ( "-- " ++ name )
    else do
      putStrLn name
      try action >>= \case
        Left ( _ :: SomeException ) -> pure ()
        Right () -> modifyIORef' passed ( Set.insert name )

failure
  :: String
  -> IO a
failure message = do
  putStrLn ( "  ✗ " ++ message )
  exitFailure

shouldBe
  :: ( Eq a, Show a )
  => a
  -> a
  -> IO ()
shouldBe x y =
  unless ( x == y )
    ( failure ( show x ++ " /= " ++ show y ) )

shouldReturn
  :: ( Eq a, Show a )
  => IO a
  -> a
  -> IO ()
shouldReturn mx y = do
  x <- mx
  x `shouldBe` y

shouldReturnRight
  :: Show a
  => IO ( Either a b )
  -> IO b
shouldReturnRight action =
  action >>= \case
    Left x ->
      failure ( "Left " ++ show x )

    Right y ->
      pure y
