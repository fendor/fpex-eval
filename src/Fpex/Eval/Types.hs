module Fpex.Eval.Types where

import qualified Control.Exception as E
import Data.Aeson
import qualified Data.Aeson.Combinators.Decode as ACD
import Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson.Internal as AesonInternal
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Fpex.Course.Types
import GHC.Generics (Generic)
import Options.Applicative
import Polysemy
import Polysemy.Reader
import System.FilePath
import Text.ParserCombinators.ReadP

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
-- Custom decoder to read results from 'tasty-grading-system'
-- ----------------------------------------------------------------------------

decodeFileTastyGradingReport :: FilePath -> IO (Either String TestSuiteResults)
decodeFileTastyGradingReport =
  ACD.eitherDecodeFileStrict decodeTastyGradingReport'

-- | Parses json output from tasty-grading-system. Expected format:
--
-- @
--   {
--       "results": [
--           {
--               "groups": [
--                   {
--                       "deductions": 0,
--                       "groups": [
--                           {
--                               "time": 31370,
--                               "name": "List comparison (different length)"
--                           },
--                           {
--                               "time": 8490,
--                               "name": "List comparison (same length)",
--                               "failure": "test/MyLibTest.hs:26:\nexpected: LT\n but got: GT"
--                           },
--                           {
--                               "time": 0,
--                               "name": "throw error",
--                               "failure": "Test\nCallStack (from HasCallStack):\n  error, called at test/MyLibTest.hs:29:9 in main:Main"
--                           },
--                           {
--                               "time": 5049731309,
--                               "name": "timeout",
--                               "failure": "Timeout"
--                           },
--                           {
--                               "time": 0,
--                               "name": "exception",
--                               "failure": "arithmetic overflow"
--                           }
--                       ],
--                       "points": 5,
--                       "tests": 5,
--                       "maximum": 9,
--                       "name": "Unit tests"
--                   }
--               ],
--               "tests": 5,
--               "name": "spec"
--           }
--       ],
--       "tests": 5,
--       "time": 5049829328,
--       "failures": 1,
--       "errors": 3
--   }
-- @
decodeTastyGradingReport' :: ACD.Decoder TestSuiteResults
decodeTastyGradingReport' = do
  groups <-
    ACD.path
      [ AesonInternal.Key "results",
        AesonInternal.Index 0,
        AesonInternal.Key "groups"
      ]
      (ACD.list decodeGroup)
  time <- ACD.key "time" ACD.integer
  pure
    TestSuiteResults
      { testGroupResults = groups,
        testSuiteTimeNs = time,
        testSuitePoints = sum $ map testGroupPoints groups
      }
  where
    decodeTestResult =
      fmap parseError (ACD.key "failure" ACD.text)
        <|> pure TestCaseResultOk

    decodeSingleTest =
      TestCaseReport
        <$> ACD.key "name" ACD.text
        <*> decodeTestResult
        <*> ACD.key "time" ACD.integer

    decodeGroup = do
      tests <- ACD.key "groups" (ACD.list decodeSingleTest)
      points <- ACD.key "points" ACD.int
      maximum' <- ACD.key "maximum" ACD.int
      deductions <- ACD.key "deductions" ACD.int
      groupName <- ACD.key "name" ACD.text
      let props = TestGroupProps groupName points deductions maximum'
      pure
        TestGroupResults
          { testGroupReports = tests,
            testGroupResultProps = props,
            testGroupPoints =
              getTestGroupPoints
                props
                (map testCaseReportResult tests)
          }

    parseError :: T.Text -> TestCaseResult
    parseError t =
      fst . head . filter (null . snd) $
        readP_to_S
          testCaseResultParser
          (T.unpack t)

testCaseResultParser :: ReadP TestCaseResult
testCaseResultParser =
  timeoutParser
    <++ expectedButGotParser
    <++ someExceptionParser

timeoutParser :: ReadP TestCaseResult
timeoutParser = do
  _ <- string "Timeout"
  eof
  pure TestCaseResultTimeout

expectedButGotParser :: ReadP TestCaseResult
expectedButGotParser = do
  _ <- munch (not . isSpace)
  skipSpaces
  _ <- string "expected:"
  skipSpaces
  expected <- munch1 (not . isSpace)
  skipSpaces
  _ <- string "but got:"
  skipSpaces
  butGot <- munch1 (not . isSpace)
  eof
  pure $ TestCaseResultExpectedButGot $ ExpectedButGot expected butGot

someExceptionParser :: ReadP TestCaseResult
someExceptionParser =
  TestCaseResultException <$> munch (const True)

-- ----------------------------------------------------------------------------
-- Utility functions based for reading and writing test suites
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

readTestSuiteResult ::
  Members [Embed IO, Reader SubmissionInfo] r =>
  Sem r (Maybe TestSuiteResults)
readTestSuiteResult = do
  sid <- asks subId
  suiteName <- asks subTestSuite
  student <- asks subStudent
  embed $ readTestSuiteResult' sid suiteName student

readTestSuiteResult' ::
  SubmissionId ->
  T.Text ->
  Student ->
  IO (Maybe TestSuiteResults)
readTestSuiteResult' sid suiteName student =
  decodeFileStrict'
    (reportSourceJsonFile sid suiteName student)

writeTestSuiteResult ::
  Members [Embed IO, Reader SubmissionInfo] r =>
  TestSuiteResults ->
  Sem r ()
writeTestSuiteResult testSuiteResults = do
  sid <- asks subId
  suiteName <- asks subTestSuite
  student <- asks subStudent
  embed $ writeTestSuiteResult' sid suiteName student testSuiteResults

writeTestSuiteResult' ::
  SubmissionId ->
  T.Text ->
  Student ->
  TestSuiteResults ->
  IO ()
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
  studentDir course student
    </> T.unpack suiteName
      <.> ("out_" <> show (getSubmissionId sid))
