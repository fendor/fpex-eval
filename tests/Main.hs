module Main where

import qualified Test.Tasty
import           Test.Tasty.Hspec
-- other tests
import qualified Course
import qualified Eval
import qualified Parser

import qualified Experiments

main :: IO ()
main = do
  Experiments.run
  test <- testSpec "fpex-eval"
    $ do
      Eval.spec
      Course.spec
      Parser.spec
  Test.Tasty.defaultMain test

