module Main where

import qualified Test.Tasty
import           Test.Tasty.Hspec
-- other tests
import qualified Course
import qualified Eval
import qualified Parser

main :: IO ()
main = do
  test <- testSpec "fpex-eval"
    $ do
      Eval.spec
      Course.spec
      Parser.spec
  Test.Tasty.defaultMain test

