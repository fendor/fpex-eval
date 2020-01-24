module Experiments where

import           TestSpecLib

import           Test.HUnit                     ( (@?=) )

run :: IO ()
run = do
    putStrLn ""
    putStrLn ""
    result <- runTestSuite 5 $ testSuite
        [ group (TestGroupProps "" 2 0 5)
                [plusOne 44 @?= 45, willError @?= 1, headOnEmptyList @?= 1]
        , group (TestGroupProps "" 2 1 1)
                [plusOne 44 @?= 45, willError @?= 1, headOnEmptyList @?= 1]
        ]
    print result
    putStrLn ""

plusOne :: Int -> Int
plusOne x = x + 1

willError :: Int
willError = error "willError"

headOnEmptyList :: Int
headOnEmptyList = head []

