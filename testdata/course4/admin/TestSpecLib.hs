{-# LANGUAGE RecordWildCards #-}

module TestSpecLib where

import           Data.Maybe                     ( fromMaybe )
import           System.Timeout                 ( timeout )
import           Test.HUnit.Lang                ( performTestCase
                                                , Assertion
                                                )
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
    deriving Show

data TestGroupResults = TestGroupResults
    { testCaseResults :: [TestCaseResult]
    , testGroupPoints :: Points
    }
    deriving Show

data TestSuiteResults = TestSuiteResults
    { testGroupResults :: [TestGroupResults]
    , testSuitePoints :: Points
    }
    deriving Show

getTestCasePoints :: TestGroupProps -> TestCaseResult -> Points
getTestCasePoints TestGroupProps {..} TestCaseResultOk = pointsPerTest
getTestCasePoints TestGroupProps {..} _                = -penalty

getTestGroupPoints :: TestGroupProps -> [TestCaseResult] -> Points
getTestGroupPoints props@TestGroupProps {..} =
    max 0 . min maximal . sum . map (getTestCasePoints props)


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
