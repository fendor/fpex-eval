module Fpex.Grade.Types where

import qualified Control.Exception as E
import Data.Aeson
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Fpex.Course.Types
import GHC.Generics (Generic)

data RunnerInfo = RunnerInfo
  { runnerInfoTimeout :: Timeout,
    runnerInfoReportOutput :: FilePath,
    runnerInfoStudentSubmission :: StudentSubmission
  }
  deriving (Show, Eq, Ord)

data ExpectedButGot = ExpectedButGot String String
  deriving (Eq, Show, Ord, Typeable, Generic)
  deriving anyclass (FromJSON, ToJSON, E.Exception)

data NotSubmitted = NotSubmitted
  deriving (Eq, Show, Typeable, Generic)
  deriving anyclass (FromJSON, ToJSON, E.Exception)

data TestGroupProps = TestGroupProps
  { label :: T.Text,
    pointsPerTest :: !Points,
    penalty :: !Points,
    upperBound :: !Points
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestCaseResult
  = TestCaseResultOk
  | TestCaseResultExpectedButGot ExpectedButGot
  | TestCaseResultException String
  | TestCaseResultTimeout
  | TestCaseResultNotSubmitted
  | TestCaseResultCompileFail
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestCaseReport = TestCaseReport
  { testCaseReportLabel :: T.Text,
    testCaseReportResult :: TestCaseResult,
    testCaseReportTimeNs :: Integer
  }
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestGroupResults = TestGroupResults
  { testGroupReports :: [TestCaseReport],
    testGroupPoints :: Points,
    testGroupResultProps :: TestGroupProps
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestSuiteResults = TestSuiteResults
  { testGroupResults :: [TestGroupResults],
    testSuitePoints :: Points,
    testSuiteTimeNs :: Integer
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Points = Int

getTestCasePoints :: TestGroupProps -> TestCaseResult -> Points
getTestCasePoints TestGroupProps {..} TestCaseResultOk = pointsPerTest
getTestCasePoints TestGroupProps {..} _ = - penalty

getTestGroupPoints :: TestGroupProps -> [TestCaseResult] -> Points
getTestGroupPoints props@TestGroupProps {..} =
  max 0 . min upperBound . sum . map (getTestCasePoints props)

newtype NotSubmittedReport = NotSubmittedReport TestSuiteResults

newtype CompileFailReport = CompileFailReport TestSuiteResults

newtype ErrorReports = ErrorReports (CompileFailReport, NotSubmittedReport)

newErrorReports :: CompileFailReport -> NotSubmittedReport -> ErrorReports
newErrorReports a b = ErrorReports (a, b)

notSubmittedReport :: ErrorReports -> NotSubmittedReport
notSubmittedReport (ErrorReports (_, b)) = b

compileFailReport :: ErrorReports -> CompileFailReport
compileFailReport (ErrorReports (a, _)) = a

isCompileFailReport :: TestSuiteResults -> Bool
isCompileFailReport =
  all
    ( (== TestCaseResultCompileFail)
        . testCaseReportResult
    )
    . getTestsSatisfying (const True)

isNotSubmittedReport :: TestSuiteResults -> Bool
isNotSubmittedReport =
  all
    ( (== TestCaseResultNotSubmitted)
        . testCaseReportResult
    )
    . getTestsSatisfying (const True)

recalculateTestPoints :: TestSuiteResults -> TestSuiteResults
recalculateTestPoints t =
  t
    { testSuitePoints = sum (map testGroupPoints newGroupResults),
      testGroupResults = newGroupResults
    }
  where
    newGroupResults = map recalculateGroup (testGroupResults t)
    recalculateGroup :: TestGroupResults -> TestGroupResults
    recalculateGroup tgroup =
      tgroup
        { testGroupPoints =
            getTestGroupPoints
              (testGroupResultProps tgroup)
              (map testCaseReportResult $ testGroupReports tgroup)
        }

allTests :: TestSuiteResults -> [TestCaseReport]
allTests = getTestsSatisfying (const True)

maxScore :: TestSuiteResults -> Points
maxScore TestSuiteResults {..} =
  sum
    (map (upperBound . testGroupResultProps) testGroupResults)

numberOfTests :: TestSuiteResults -> Int
numberOfTests =
  length . getTestsSatisfying (const True)

correctTests :: TestSuiteResults -> Int
correctTests =
  length . getTestsSatisfying isPassedTestCaseResult

notSubmittedTests :: TestSuiteResults -> Int
notSubmittedTests =
  length . getTestsSatisfying (== TestCaseResultNotSubmitted)

failedTests :: TestSuiteResults -> Int
failedTests =
  length . getTestsSatisfying isFailedTestCaseResult

timeoutTests :: TestSuiteResults -> Int
timeoutTests =
  length . getTestsSatisfying (== TestCaseResultTimeout)

getTestsSatisfying ::
  (TestCaseResult -> Bool) ->
  TestSuiteResults ->
  [TestCaseReport]
getTestsSatisfying p TestSuiteResults {..} =
  concatMap
    (filter (p . testCaseReportResult) . testGroupReports)
    testGroupResults

isPassedTestCaseResult :: TestCaseResult -> Bool
isPassedTestCaseResult TestCaseResultOk = True
isPassedTestCaseResult _ = False

isFailedTestCaseResult :: TestCaseResult -> Bool
isFailedTestCaseResult (TestCaseResultExpectedButGot _) = True
isFailedTestCaseResult (TestCaseResultException _) = True
isFailedTestCaseResult _ = False

newtype Timeout = Timeout {getTimeout :: Float}
  deriving (Show, Generic)
  deriving newtype (Eq, Num, Ord)
