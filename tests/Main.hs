module Main where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Fpex Unit Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ testCase "" $ pure ()
    ]
