module Fpex.Eval.Types
  ( TestSuite (..),
    TestSuiteResults (..),
    TestCaseReport (..),
    TestCaseResult (..),
    TestGroup (..),
    TestGroupResults (..),
    TestGroupProps (..),
    SubmissionId (..),
    Points,
    Timeout(..),
    ExpectedButGot(..),
    studentSourceFile,
    assignmentCollectDir,
    assignmentCollectStudentDir,
    assignmentCollectStudentFile,
    maxScore,
    reportSourceJsonFile,
    reportPublishFile,
    recalculateTestPoints,
    correctTests,
    notSubmittedTests,
    failedTests,
    timeoutTests,
    studentDir,
    readTestSuiteResult,
    writeTestSuiteResult,

  )
where

import Data.Aeson
import Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Fpex.Course.Types
import GHC.Generics (Generic)
import System.FilePath
import TestSpec as Spec

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

maxScore :: TestSuiteResults -> Points
maxScore TestSuiteResults {..} = sum (map (upperBound . testGroupResultProps) testGroupResults)

correctTests :: TestSuiteResults -> Int
correctTests testSuiteResults =
  length $ getTestsSatisfying (== TestCaseResultOk) testSuiteResults

notSubmittedTests :: TestSuiteResults -> Int
notSubmittedTests testSuiteResults =
  length $ getTestsSatisfying (== TestCaseResultNotSubmitted) testSuiteResults

failedTests :: TestSuiteResults -> Int
failedTests testSuiteResults =
  length $ getTestsSatisfying isFailedTestCaseResult testSuiteResults

timeoutTests :: TestSuiteResults -> Int
timeoutTests testSuiteResults =
  length $ getTestsSatisfying (== TestCaseResultTimeout) testSuiteResults

getTestsSatisfying :: (TestCaseResult -> Bool) -> TestSuiteResults -> [TestCaseReport]
getTestsSatisfying p TestSuiteResults {..} =
    concatMap
      (filter (p . testCaseReportResult) . testGroupReports)
      testGroupResults

isFailedTestCaseResult :: TestCaseResult -> Bool
isFailedTestCaseResult (TestCaseResultExpectedButGot _) = True
isFailedTestCaseResult (TestCaseResultException _) = True
isFailedTestCaseResult _ = False

newtype Timeout = Timeout {getTimeout :: Float}
  deriving (Show, Generic)
  deriving newtype (Eq, Num, Ord)

newtype SubmissionId = SubmissionId {getSubmissionId :: Int}
  deriving (Show, Generic)
  deriving newtype (Eq, Num, Ord)

studentDir :: Course -> Student -> FilePath
studentDir Course {courseRootDir} Student {studentId} =
  courseRootDir </> T.unpack studentId

-- | Filename of the submission file
studentSourceFile :: Course -> T.Text -> Student -> FilePath
studentSourceFile course suiteName student =
  studentDir course student </> T.unpack suiteName <.> "hs"

assignmentCollectDir :: SubmissionId -> T.Text -> FilePath
assignmentCollectDir sid suiteName =
  ( T.unpack suiteName
      <> "-"
      <> show (getSubmissionId sid)
  )

readTestSuiteResult :: SubmissionId -> T.Text -> Student -> IO TestSuiteResults
readTestSuiteResult sid suiteName student = do
  Just suite <- decodeFileStrict (reportSourceJsonFile sid suiteName student)
  pure suite

writeTestSuiteResult :: SubmissionId -> T.Text -> Student -> TestSuiteResults -> IO ()
writeTestSuiteResult sid suiteName student testSuiteResults =
  LBS.writeFile (reportSourceJsonFile sid suiteName student) (Aeson.encodePretty testSuiteResults)

assignmentCollectStudentDir ::
  SubmissionId -> T.Text -> Student -> FilePath
assignmentCollectStudentDir sid suiteName student =
  assignmentCollectDir sid suiteName </> T.unpack (studentId student)

assignmentCollectStudentFile :: SubmissionId -> T.Text -> Student -> FilePath
assignmentCollectStudentFile sid suiteName student =
  assignmentCollectStudentDir sid suiteName student
    </> T.unpack suiteName <.> "hs"

reportSourceJsonFile :: SubmissionId -> T.Text -> Student -> FilePath
reportSourceJsonFile sid suiteName student =
  assignmentCollectStudentDir sid suiteName student </> "report.json"

reportPublishFile :: SubmissionId -> Course -> T.Text -> Student -> FilePath
reportPublishFile sid course suiteName student =
  studentDir course student </> T.unpack suiteName <.> ("out_" <> show (getSubmissionId sid))
