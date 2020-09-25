module Fpex.Grade.Types where

import qualified Control.Exception as E
import Data.Aeson
import Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Fpex.Course.Types
import GHC.Generics (Generic)
import Polysemy
import System.FilePath
import Polysemy.Internal

data SubmissionInfo = SubmissionInfo
  { subStudent :: Student,
    subId :: SubmissionId,
    subTestSuite :: T.Text
  }
  deriving (Show, Eq, Ord)

data RunnerInfo = RunnerInfo
  { runnerInfoTimeout :: Timeout,
    runnerInfoReportOutput :: FilePath
  }
  deriving (Show, Eq, Ord)

data ExpectedButGot = ExpectedButGot String String
  deriving (Eq, Show, Typeable, Generic)
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
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestCaseReport = TestCaseReport
  { testCaseReportLabel :: T.Text,
    testCaseReportResult :: TestCaseResult,
    testCaseReportTimeNs :: Integer
  }
  deriving (Eq, Show, Generic)
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

maxScore :: TestSuiteResults -> Points
maxScore TestSuiteResults {..} =
  sum
    (map (upperBound . testGroupResultProps) testGroupResults)

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

getTestsSatisfying ::
  (TestCaseResult -> Bool) ->
  TestSuiteResults ->
  [TestCaseReport]
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


-- ----------------------------------------------------------------------------
-- Utility functions based for reading and writing test suites
-- ----------------------------------------------------------------------------

data TestSuiteStorage m a where
  WriteTestSuiteResult :: SubmissionInfo -> TestSuiteResults -> TestSuiteStorage m ()
  ReadTestSuiteResult :: SubmissionInfo -> TestSuiteStorage m (Maybe TestSuiteResults)

writeTestSuiteResult :: Member TestSuiteStorage r => SubmissionInfo -> TestSuiteResults -> Sem r ()
writeTestSuiteResult info s = send $ WriteTestSuiteResult info s

readTestSuiteResult :: Member TestSuiteStorage r => SubmissionInfo -> Sem r (Maybe TestSuiteResults)
readTestSuiteResult info = send $ ReadTestSuiteResult info

runTestSuiteStorageFileSystem :: Member (Embed IO) r => Sem (TestSuiteStorage : r) a ->  Sem (r) a
runTestSuiteStorageFileSystem = interpret $ \case
  WriteTestSuiteResult SubmissionInfo {..} results ->
    embed $ writeTestSuiteResultIO subId subTestSuite subStudent results
  ReadTestSuiteResult SubmissionInfo {..} ->
    embed $ readTestSuiteResultIO subId subTestSuite subStudent

readTestSuiteResultIO ::
  SubmissionId ->
  T.Text ->
  Student ->
  IO (Maybe TestSuiteResults)
readTestSuiteResultIO sid suiteName student =
  decodeFileStrict'
    (reportSourceJsonFile sid suiteName student)

writeTestSuiteResultIO ::
  SubmissionId ->
  T.Text ->
  Student ->
  TestSuiteResults ->
  IO ()
writeTestSuiteResultIO sid suiteName student testSuiteResults =
  LBS.writeFile
    (reportSourceJsonFile sid suiteName student)
    (Aeson.encodePretty testSuiteResults)

-- ----------------------------------------------------------------------------
-- Pure functions for extracting filepath information
-- ----------------------------------------------------------------------------

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
  studentDir course student
    </> T.unpack suiteName
      <.> ("out_" <> show (getSubmissionId sid))
