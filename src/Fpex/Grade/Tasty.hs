module Fpex.Grade.Tasty where

import Control.Applicative
import Control.Monad.Extra (unlessM)
import qualified Data.Aeson.Combinators.Decode as ACD
import Data.Aeson.Internal as AesonInternal
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Function
import Data.List (isInfixOf)
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade (RunTestSuite (..), RunnerError (..))
import Fpex.Grade.Paths
import Fpex.Grade.Result
import Fpex.Grade.Types
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import System.Directory
import System.FilePath
import qualified System.Process.Typed as Proc
import Text.ParserCombinators.ReadP

runTastyTestSuite ::
  Members
    [ Error RunnerError,
      Embed IO,
      Reader Course,
      Reader RunnerInfo
    ]
    r =>
  Sem (RunTestSuite : r) a ->
  Sem r a
runTastyTestSuite = interpret $ \case
  RunTestSuite SubmissionInfo {..} -> do
    let sid = subId
    let suiteName = subName
    let student = subStudent
    let targetDir = assignmentCollectStudentDir' sid suiteName student
    studentSubmission <- asks runnerInfoStudentSubmission
    let targetFile = assignmentCollectStudentFile sid suiteName studentSubmission student
    ghciOptions <- asks courseGhciOptions
    ghciEnv <- asks ghciEnvironmentLocation
    reportOutput <- asks runnerInfoReportOutput
    testTimeout <- asks runnerInfoTimeout
    unlessM (embed $ doesFileExist targetFile) $ throw NoSubmission
    let procArgs =
          [ "../Main.hs",
            "-package-env",
            ghciEnv,
            "-i",
            "-i.",
            "-i.."
          ]
            ++ ghciOptions
            ++ [ "-e",
                 unwords
                   [ ":main",
                     "-j",
                     "1",
                     "-t",
                     show (getTimeout testTimeout),
                     "--grading-json",
                     reportOutput
                   ]
               ]
        procConfig =
          Proc.proc "ghci" procArgs
            & Proc.setWorkingDir targetDir
    (_procRes, sout, serr) <- Proc.readProcess procConfig
    -- Write logs to files
    embed $ do
      LBS.writeFile (targetDir </> "stderr.log") serr
      LBS.writeFile (targetDir </> "stdout.log") sout
    -- case procRes of
    --   ExitSuccess -> return ()
    --   ExitFailure _ -> throw $ RunnerInternalError (T.pack $ show procConfig) serr
    unlessM (embed $ doesFileExist (targetDir </> reportOutput)) $ throw (RunnerInternalError (studentId student) serr)
    decodeResult <- embed $ decodeFileTastyGradingReport (targetDir </> reportOutput)
    case decodeResult of
      Left msg -> throw $ FailedToDecodeJsonResult msg
      Right s -> pure s

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
      fst . head $
        readP_to_S
          testCaseResultParser
          (T.unpack t)

testCaseResultParser :: ReadP TestCaseResult
testCaseResultParser =
  timeoutParser
    <++ expectedButGotParser
    <++ notSubmittedParser
    <++ someExceptionParser

timeoutParser :: ReadP TestCaseResult
timeoutParser = do
  _ <- string "Timeout"
  eof
  pure TestCaseResultTimeout

notSubmittedParser :: ReadP TestCaseResult
notSubmittedParser = do
  s <- look
  if isInfixOf "Variable not in scope" s
    then pure TestCaseResultNotSubmitted
    else pfail

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
