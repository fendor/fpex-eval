module Main where

import GradeResultParser (goldenParserTests)
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Fpex Tests" [goldenParserTests]
