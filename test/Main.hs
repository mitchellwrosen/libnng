module Main where

import Test.Tasty

import qualified LibnngTests

main :: IO ()
main =
  defaultMain
    ( testGroup
        "tests"
        [ testGroup "libnng" LibnngTests.tests
        -- , testGroup "nng" nngTests
        ]
    )
