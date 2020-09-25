module Fpex.Grade.Tasty where

import qualified Data.Aeson.Combinators.Decode as ACD
import Data.Aeson.Internal as AesonInternal
import Data.Char
import qualified Data.Text as T
import Control.Applicative
import Text.ParserCombinators.ReadP
import Fpex.Grade.Types
import Data.List (isInfixOf)


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
