module Analyser (analyserTests) where

import Data.Monoid
import qualified Data.Set as Set
import Fpex.Grade.Analysis
import Test.Tasty
import Test.Tasty.HUnit

analyserTests :: TestTree
analyserTests = testGroup "Analyser Tests" [analyserMonoidInstance]

analyserMonoidInstance :: TestTree
analyserMonoidInstance =
  testGroup
    "Analyser Monoid instance"
    [ testCase "" $ do
        AnalysisState
          { reachedFullPoints = Any False,
            passedAllTests = Any False,
            testsPassed = Set.fromList [1 .. 3],
            pointRegression = []
          }
          <> AnalysisState
            { reachedFullPoints = Any True,
              passedAllTests = Any True,
              testsPassed = Set.fromList [2 .. 4],
              pointRegression = []
            }
          @?= AnalysisState
            { reachedFullPoints = Any True,
              passedAllTests = Any True,
              testsPassed = Set.fromList [1 .. 4],
              pointRegression = []
            }
    ]
