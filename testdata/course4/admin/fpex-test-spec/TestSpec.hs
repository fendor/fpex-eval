{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module TestSpec where

import           Data.Maybe                     ( fromMaybe )
import           System.Timeout                 ( timeout )
import           System.IO                      ( FilePath )
import           Test.HUnit.Lang                ( performTestCase
                                                , Assertion
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Aeson.Encode.Pretty      as Aeson
import qualified Data.ByteString.Lazy          as BL

import qualified Test.HUnit.Lang               as HUnit

group :: TestGroupProps -> [TestCase] -> TestGroup
group = TestGroup

testSuite :: [TestGroup] -> TestSuite
testSuite = TestSuite

type Points = Int
type TimeoutSecs = Int

type TestCase = Assertion

data TestGroupProps = TestGroupProps
    { label :: String
    , pointsPerTest :: !Points
    , penalty :: !Points
    , maximal :: !Points
    }
    deriving Show

data TestGroup = TestGroup
    { testGroupProps :: TestGroupProps
    , testCases :: [TestCase]
    }

newtype TestSuite = TestSuite
    { testSuiteGroups :: [TestGroup]}

data TestCaseResult
    = TestCaseResultOk
    | TestCaseResultNok String
    | TestCaseResultException String
    | TestCaseResultTimeout
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestGroupResults = TestGroupResults
    { testCaseResults :: [TestCaseResult]
    , testGroupPoints :: Points
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestSuiteResults = TestSuiteResults
    { testGroupResults :: [TestGroupResults]
    , testSuitePoints :: Points
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

getTestCasePoints :: TestGroupProps -> TestCaseResult -> Points
getTestCasePoints TestGroupProps {..} TestCaseResultOk = pointsPerTest
getTestCasePoints TestGroupProps {..} _                = -penalty

getTestGroupPoints :: TestGroupProps -> [TestCaseResult] -> Points
getTestGroupPoints props@TestGroupProps {..} =
    max 0 . min maximal . sum . map (getTestCasePoints props)

writeTestSuiteResults :: FilePath -> TestSuiteResults -> IO ()
writeTestSuiteResults outFile = BL.writeFile outFile . Aeson.encodePretty

-- runners

runTestCase :: TimeoutSecs -> Assertion -> IO TestCaseResult
runTestCase timeoutSecs testCase = fromMaybe TestCaseResultTimeout <$> timeout
    (timeoutSecs * 1000 * 1000)
    (hUnitResultToTestCaseResult <$> performTestCase testCase)

hUnitResultToTestCaseResult :: HUnit.Result -> TestCaseResult
hUnitResultToTestCaseResult HUnit.Success       = TestCaseResultOk
hUnitResultToTestCaseResult (HUnit.Failure _ s) = TestCaseResultNok s
hUnitResultToTestCaseResult (HUnit.Error   _ s) = TestCaseResultException s

runTestGroup :: TimeoutSecs -> TestGroup -> IO TestGroupResults
runTestGroup timeoutSecs TestGroup {..} = do
    testCaseResults <- mapM (runTestCase timeoutSecs) testCases
    let testGroupPoints = getTestGroupPoints testGroupProps testCaseResults
    return TestGroupResults { .. }

runTestSuite :: TimeoutSecs -> TestSuite -> IO TestSuiteResults
runTestSuite timeoutSecs TestSuite {..} = do
    testGroupResults <- mapM (runTestGroup timeoutSecs) testSuiteGroups
    let testSuitePoints = sum . map testGroupPoints $ testGroupResults
    return TestSuiteResults { .. }
