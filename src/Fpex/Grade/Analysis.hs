module Fpex.Grade.Analysis where

import Colourista.IO
import Colourista.Pure
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Semigroup.Generic
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade.Storage
import Fpex.Grade.Types
import GHC.Generics (Generic)
import Polysemy
import Polysemy.Internal (send)
import Polysemy.Reader
import Polysemy.State (State)
import qualified Polysemy.State as State

type Warning = T.Text

newtype AnalysisReport = AnalysisReport AnalysisState

data Analyser m a where
  AnalyseTestSuiteResult :: SubmissionInfo -> TestSuiteResults -> Analyser m (Maybe Warning)
  FinalAnalysisReport :: Analyser m AnalysisReport

analyseTestSuite :: Member Analyser r => SubmissionInfo -> TestSuiteResults -> Sem r (Maybe Warning)
analyseTestSuite sinfo t = send (AnalyseTestSuiteResult sinfo t)

finalAnalysisReport :: Member Analyser r => Sem r AnalysisReport
finalAnalysisReport = send FinalAnalysisReport

data AnalysisState = AnalysisState
  { reachedFullPoints :: !Any,
    passedAllTests :: !Any,
    testsPassed :: Map Int TestCaseReport,
    pointRegression :: ![(SubmissionInfo, Reason)]
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid AnalysisState)

data Reason
  = ReasonNotSubmitted
  | ReasonCompileFail
  | ReasonFewerPoints Points Points
  deriving (Show, Eq, Ord)

runStatefulAnalyser ::
  Members
    [ TestSuiteStorage,
      State AnalysisState
    ]
    r =>
  Sem (Analyser : r) a ->
  Sem r a
runStatefulAnalyser = interpret $ \case
  AnalyseTestSuiteResult sinfo t -> do
    let reachedFullPoints = Any $ maxScore t <= testSuitePoints t
        passedAllTests = Any $ numberOfTests t == correctTests t
        oldSubmission = previousSubmission sinfo
    let testsPassed = Map.fromList $ zip [1 ..] $ getTestsSatisfying isPassedTestCaseResult t
    pointRegression <-
      doesTestSuiteResultExist oldSubmission >>= \case
        False -> pure []
        True ->
          readTestSuiteResult oldSubmission >>= \case
            oldResult
              | testSuitePoints t < testSuitePoints oldResult ->
                case () of
                  _
                    | isNotSubmittedReport t ->
                      pure [(sinfo, ReasonNotSubmitted)]
                    | isCompileFailReport t ->
                      pure [(sinfo, ReasonCompileFail)]
                    | otherwise ->
                      pure [(sinfo, ReasonFewerPoints (testSuitePoints oldResult) (testSuitePoints t))]
              | otherwise -> pure []

    State.modify (<> AnalysisState {..})
    pure Nothing
  FinalAnalysisReport -> AnalysisReport <$> State.get

previousSubmission :: SubmissionInfo -> SubmissionInfo
previousSubmission s = s {subId = subId s - 1}

printFinalAnalysisReport :: Members [Embed IO, Reader ErrorReports] r => AnalysisReport -> Sem r ()
printFinalAnalysisReport (AnalysisReport AnalysisState {..}) = do
  NotSubmittedReport someErrorReport <- asks notSubmittedReport
  let numberedTests = Map.fromList $ zip [1 ..] $ allTests someErrorReport
      testsNoOnePassed = Map.difference numberedTests testsPassed

      formatSubmissionInfo SubmissionInfo {..} =
        "Student " <> formatWith [bold] (studentId subStudent) <> ":"

  unless (getAny reachedFullPoints) $ do
    embed $ warningMessage "Not a single Student reached the full score!"

  unless (getAny passedAllTests) $ do
    embed $ infoMessage "Not a single Student passed all tests!"

  when (not . null $ pointRegression) $ do
    embed $ warningMessage "There were some Regressions in the Points"
  forM_ pointRegression $ \(sinfo, reason) ->
    case reason of
      ReasonNotSubmitted ->
        embed $ infoMessage $ formatSubmissionInfo sinfo <> " No Assignment has been submitted this time."
      ReasonCompileFail ->
        embed $ infoMessage $ formatSubmissionInfo sinfo <> " Assignment failed to compile."
      ReasonFewerPoints oldPoints newPoints ->
        embed $
          infoMessage $
            " Previously " <> formatWith [red] (T.pack $ show oldPoints) <> " Points, now "
              <> formatWith [yellow] (T.pack $ show newPoints)
              <> " Points"

  when (not . null $ testsNoOnePassed) $ do
    embed $ warningMessage "There are test cases that not a single student passed!"
  forM_ (Map.assocs testsNoOnePassed) $ \(num, testCase) ->
    embed $
      skipMessage $
        "Test case " <> formatWith [magenta] (T.pack $ show num)
          <> ( if T.null (testCaseReportLabel testCase)
                 then ""
                 else " with label: \"" <> testCaseReportLabel testCase <> "\""
             )

  when (getAny (reachedFullPoints <> passedAllTests) && null pointRegression && null testsNoOnePassed) $ do
    embed $ successMessage "Report has no warnings"
  pure ()