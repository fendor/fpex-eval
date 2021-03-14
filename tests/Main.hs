module Main where

import GradeResultParser (goldenParserTests)
import Test.Tasty
import Report (goldenReportTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Fpex Tests" [goldenParserTests, goldenReportTests]
