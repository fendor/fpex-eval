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
    ErrorReports,
    NotSubmittedReport(..),
    CompileFailReport(..),
    SubmissionInfo(..),
    newErrorReports,
    notSubmittedReport,
    compileFailReport,
    isCompileFailReport,
    isNotSubmittedReport,
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
    readTestSuiteResult',
    writeTestSuiteResult',

  )
where

import Polysemy
import Polysemy.Reader
import Data.Aeson
import Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Fpex.Course.Types
import GHC.Generics (Generic)
import System.FilePath
import Data.Typeable (Typeable)
import qualified Control.Exception as E

data SubmissionInfo = SubmissionInfo
  { subStudent :: Student
  , subId :: SubmissionId
  , subTestSuite :: T.Text
  }

data ExpectedButGot = ExpectedButGot String String
  deriving (Eq, Show, Typeable, Generic)
  deriving anyclass (FromJSON, ToJSON, E.Exception)

data NotSubmitted = NotSubmitted
  deriving (Eq, Show, Typeable, Generic)
  deriving anyclass (FromJSON, ToJSON, E.Exception)

type TestCase = (String, IO ())

data TestGroupProps = TestGroupProps
  { label :: String,
    pointsPerTest :: !Points,
    penalty :: !Points,
    upperBound :: !Points
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TestGroup = TestGroup
  { testGroupProps :: TestGroupProps,
    testCases :: [TestCase]
  }

newtype TestSuite = TestSuite
  {testSuiteGroups :: [TestGroup]}

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
  { testCaseReportLabel :: String,
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
    testSuitePoints :: Points
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
isCompileFailReport = all (( == TestCaseResultCompileFail) . testCaseReportResult ) . getTestsSatisfying (const True)

isNotSubmittedReport :: TestSuiteResults -> Bool
isNotSubmittedReport = all (( == TestCaseResultNotSubmitted) . testCaseReportResult ) . getTestsSatisfying (const True)

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

readTestSuiteResult :: Members [Embed IO, Reader SubmissionInfo] r => Sem r (Maybe TestSuiteResults)
readTestSuiteResult = do
  sid <- asks subId
  suiteName <- asks subTestSuite
  student <- asks subStudent
  embed $ readTestSuiteResult' sid suiteName student

readTestSuiteResult' :: SubmissionId -> T.Text -> Student -> IO (Maybe TestSuiteResults)
readTestSuiteResult' sid suiteName student = decodeFileStrict' (reportSourceJsonFile sid suiteName student)

writeTestSuiteResult ::  Members [Embed IO, Reader SubmissionInfo] r => TestSuiteResults -> Sem r ()
writeTestSuiteResult  testSuiteResults = do
  sid <- asks subId
  suiteName <- asks subTestSuite
  student <- asks subStudent
  embed $ writeTestSuiteResult' sid suiteName student testSuiteResults

writeTestSuiteResult' :: SubmissionId -> T.Text -> Student -> TestSuiteResults -> IO ()
writeTestSuiteResult' sid suiteName student testSuiteResults =
  LBS.writeFile
    (reportSourceJsonFile sid suiteName student)
    (Aeson.encodePretty testSuiteResults)

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
