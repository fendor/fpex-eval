module Fpex.Grade.Analysis where

import Colourista.IO
import Colourista.Pure
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Semigroup.Generic
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade.Result
import Fpex.Grade.Storage
import GHC.Generics (Generic)
import Polysemy
import Polysemy.Internal (send)
import Polysemy.Reader
import Polysemy.State (State)
import qualified Polysemy.State as State

type Warning = T.Text

data Analyser m a where
  AnalyseTestSuiteResult :: SubmissionInfo -> TestSuiteResults -> Analyser m (Maybe Warning)

analyseTestSuite :: Member Analyser r => SubmissionInfo -> TestSuiteResults -> Sem r (Maybe Warning)
analyseTestSuite sinfo t = send (AnalyseTestSuiteResult sinfo t)

data AnalysisState = AnalysisState
  { reachedFullPoints :: !Any,
    passedAllTests :: !Any,
    testsPassed :: Set Int,
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
    [ Storage,
      State AnalysisState
    ]
    r =>
  Sem (Analyser : r) a ->
  Sem r a
runStatefulAnalyser = interpret $ \case
  AnalyseTestSuiteResult sinfo t -> do
    oldResultReport <- readTestSuiteResultM sinfo
    let analysisReport = analyseTestResultPure sinfo t oldResultReport
    State.modify (<> analysisReport)
    pure Nothing

analyseTestResultPure ::
  SubmissionInfo ->
  TestSuiteResults ->
  Maybe TestSuiteResults ->
  AnalysisState
analyseTestResultPure sinfo t mOldTestSuite =
  let reachedFullPoints = Any $ maxScore t <= testSuitePoints t
      passedAllTests = Any $ numberOfTests t == correctTests t
      testsPassed =
        Set.fromList $
          mapMaybe
            ( \(tid, tc) ->
                case isPassedTestCaseResult $ testCaseReportResult tc of
                  False -> Nothing
                  True -> Just tid
            )
            $ zip [1 ..] (allTests t)
      pointRegression = case mOldTestSuite of
        Just oldResult
          | testSuitePoints t < testSuitePoints oldResult ->
            case () of
              _
                | isNotSubmittedReport t -> [(sinfo, ReasonNotSubmitted)]
                | isCompileFailReport t -> [(sinfo, ReasonCompileFail)]
                | otherwise -> [(sinfo, ReasonFewerPoints (testSuitePoints oldResult) (testSuitePoints t))]
          | otherwise -> []
        Nothing -> []
   in AnalysisState {..}

previousSubmission :: SubmissionInfo -> SubmissionInfo
previousSubmission s = s {subId = subId s - 1}

printCurrentAnalysisReport :: Members [Embed IO, Reader ErrorReports] r => AnalysisState -> Sem r ()
printCurrentAnalysisReport AnalysisState {..} = do
  NotSubmittedReport someErrorReport <- asks notSubmittedReport
  let numberedTests = Map.fromList $ zip [1 ..] $ allTests (canonicaliseTestSuiteResults someErrorReport)
      testsNoOnePassed = numberedTests `Map.withoutKeys` testsPassed

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
            formatSubmissionInfo sinfo
              <> " Previously "
              <> formatWith [red] (T.pack $ show oldPoints)
              <> " Points, now "
              <> formatWith [yellow] (T.pack $ show newPoints)
              <> " Points"

  when (not . null $ testsNoOnePassed) $ do
    embed $ warningMessage "There are test cases that not a single student passed!"
  forM_ (Map.toAscList testsNoOnePassed) $ \(num, testCase) ->
    embed $
      skipMessage $
        "Test case " <> formatWith [magenta] (T.pack $ show num)
          <> ( if T.null (testCaseReportLabel testCase)
                 then ""
                 else " with label: \"" <> testCaseReportLabel testCase <> "\""
             )

  when (getAny (reachedFullPoints <> passedAllTests) && null pointRegression && null testsNoOnePassed) $ do
    embed $ successMessage "Report has no warnings"
