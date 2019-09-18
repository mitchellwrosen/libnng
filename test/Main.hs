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

  test "nng_close returns Right on open socket" do
    socket <- shouldReturnRight nng_req0_open
    nng_close socket `shouldReturn` Right ()

  test "nng_close returns Left 7 on closed socket" do
    socket <- shouldReturnRight nng_req0_open
    nng_close socket `shouldReturn` Right ()
    nng_close socket `shouldReturn` Left 7

  test "nng_dial returns Left 3 on bogus url" do
    with_nng_req0 \socket ->
      nng_dial socket "foo" 0 `shouldReturn` Left 3

  test "nng_dial returns Left 6 on endpoint that's not listening" do
    with_nng_req0 \socket ->
      nng_dial socket "inproc://foo" 0 `shouldReturn` Left 6

  -- TODO nng_dial protocol error

  test "nng_dial returns Right" do
    with_nng_req0 \req ->
      with_nng_rep0 \rep -> do
        shouldReturnRight ( nng_listen_ rep "inproc://foo" 0 )
        shouldReturnRight ( nng_dial_ req "inproc://foo" 0 )

  test "nng_dialer_close returns Right on open dialer" do
    with_nng_req0 \req ->
      with_nng_rep0 \rep -> do
        shouldReturnRight ( nng_listen_ rep "inproc://foo" 0 )
        dialer <- shouldReturnRight ( nng_dial req "inproc://foo" 0 )
        nng_dialer_close dialer `shouldReturn` Right ()

  test "nng_listen returns Left 3 on bogus url" do
    with_nng_rep0 \socket ->
      nng_listen socket "foo" 0 `shouldReturn` Left 3

  test "nng_listen returns Right" do
    with_nng_rep0 \socket -> do
      shouldReturnRight ( nng_listen_ socket "inproc://foo" 0 )

  -- TODO nng_listen protocol error

  test "nng_rep0_open returns Right" do
    socket <- shouldReturnRight nng_rep0_open
    nng_close socket `shouldReturn` Right ()

  test "nng_req0_open returns Right" do
    socket <- shouldReturnRight nng_req0_open
    nng_close socket `shouldReturn` Right ()

  traverse_
    ( \(n, s) ->
        test ( "nng_strerror " ++ show n ) do
          nng_strerror n `shouldBe` s
    )
    [ ( 0,  "Hunky dory"         )
    , ( 1,  "Interrupted"        )
    , ( 2,  "Out of memory"      )
    , ( 3,  "Invalid argument"   )
    , ( 4,  "Resource busy"      )
    , ( 5,  "Timed out"          )
    , ( 6,  "Connection refused" )
    , ( 7,  "Object closed"      )
    , ( 8,  "Try again"          )
    , ( 9,  "Not supported"      )
    , ( 10, "Address in use"     )
    , ( 11, "Incorrect state"    )
    , ( 12, "Entry not found"    )
      -- Well these are boring to test!
    ]

  test "nng_version is 1.1.1" do
    nng_version `shouldBe` "1.1.1"

  do
    names <- readIORef passed
    writeFile ".testcache" ( unlines ( Set.toList names ) )

with_nng_rep0
  :: ( NngSocket -> IO a )
  -> IO a
with_nng_rep0 action =
  bracket
    nng_rep0_open
    ( either
        ( const ( pure () ) )
        ( void . nng_close )
    )
    ( either
        ( \n -> throwIO ( userError ( "with_nng_rep0: " ++ show n ) ) )
        action
    )

with_nng_req0
  :: ( NngSocket -> IO a )
  -> IO a
with_nng_req0 action =
  bracket
    nng_req0_open
    ( either
        ( const ( pure () ) )
        ( void . nng_close )
    )
    ( either
        ( \n -> throwIO ( userError ( "with_nng_req0: " ++ show n ) ) )
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
