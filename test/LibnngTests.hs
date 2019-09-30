module LibnngTests
  ( tests
  ) where

-- import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Data.Either
-- import Data.Foldable
-- import Data.IORef
-- import Data.Set (Set)
import Foreign.C.String
-- import System.Exit
-- import System.IO.Unsafe (unsafePerformIO)
-- import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

import Libnng


tests :: [ TestTree ]
tests =
  [ testGroup "close"        test_close
  , testGroup "dial"         test_dial
  , testGroup "dialer_close" test_dialer_close
  , testGroup "listen"       test_listen
  , testGroup "rep0_open"    test_rep0_open
  , testGroup "req0_open"    test_req0_open
  , testGroup "strerror"     test_strerror
  , testGroup "version"      test_version
  ]

test_close :: [ TestTree ]
test_close =
  [ testCase "close returns Right on open socket" do
      Right socket <- req0_open
      result <- close socket
      result @?= Right ()

  , testCase "close returns Left 7 on closed socket" do
      Right socket <- req0_open
      Right () <- close socket
      result <- close socket
      result @?= Left 7
  ]

test_dial :: [ TestTree ]
test_dial =
  [ testCase "dial returns Left 3 on bogus address" do
      with_req0 \socket ->
        withCString "foo" \address -> do
          result <- dial socket address 0
          result @?= Left 3

  , testCase "dial returns Left 6 on endpoint that's not listening" do
      with_req0 \socket ->
        withCString "inproc://foo" \address -> do
          result <- dial socket address 0
          result @?= Left 6

  -- TODO dial protocol error

  , testCase "dial returns Right" do
      with_req0 \req ->
        with_rep0 \rep ->
          withCString "inproc://foo" \address -> do
            Right () <- listen_ rep address 0
            result <- dial_ req address 0
            isRight result @? show result
  ]

test_dialer_close :: [ TestTree ]
test_dialer_close =
  [ testCase "dialer_close returns Right on open dialer" do
      with_req0 \req ->
        with_rep0 \rep ->
          withCString "inproc://foo" \address -> do
            Right () <- listen_ rep address 0
            Right dialer <- dial req address 0
            result <- dialer_close dialer
            result @?= Right ()
  ]

test_listen :: [ TestTree ]
test_listen =
  [ testCase "listen returns Left 3 on bogus address" do
      with_rep0 \socket ->
        withCString "foo" \address -> do
          result <- listen socket address 0
          result @?= Left 3

  , testCase "listen returns Right" do
      with_rep0 \socket ->
        withCString "inproc://foo" \address -> do
          result <- listen_ socket address 0
          result @?= Right ()

  -- TODO listen protocol error
  ]

test_rep0_open :: [ TestTree ]
test_rep0_open =
  [ testCase "rep0_open returns Right" do
      rep0_open >>= \case
        Left err ->
          assertFailure ( show err )
        Right socket -> do
          Right () <- close socket
          pure ()
  ]

test_req0_open :: [ TestTree ]
test_req0_open =
  [ testCase "req0_open returns Right" do
      req0_open >>= \case
        Left err ->
          assertFailure ( show err )
        Right socket -> do
          Right () <- close socket
          pure ()
  ]

test_strerror :: [ TestTree ]
test_strerror =
  map
    ( \(n, s) ->
        testCase ( "strerror " ++ show n ) do
          s' <- peekCString ( strerror n )
          s' @?= s
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

test_version :: [ TestTree ]
test_version =
  [ testCase "version is 1.1.1" do
      result <- peekCString version
      result @?= "1.1.1"
  ]

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
