module Main (main) where


import Assignment1
import Control.Exception
import Test.Tasty
import Test.Tasty.Grade
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [composeReporters consoleTestReporter jsonRunner] spec

spec :: TestTree
spec =
  testGroup
    "TestSuite1 Spec"
    [ testGroupPoints 5 0 14 fibTests
    , testGroupPoints 5 0 5 barTests
    ]

fibTests :: TestTree
fibTests =
  testGroup
    "Fib tests"
    [ testCase "Basic Fib Test 1" $
        fib 0 @?= 1,
      testCase "Basic Fib Test 2" $
        fib 1 @?= 1,
      testCase "Basic Fib Test 3" $
        fib 30 @?= 1346269
    ]

barTests :: TestTree
barTests =
  testGroup
    "Bar tests"
    [ testCase "Basic Bar Test 1" $
        bar @?= 2
    ]

