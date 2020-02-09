{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestSpec where

import           Data.Maybe                     ( fromMaybe )
import           System.Timeout                 ( timeout )
import           System.IO                      ( FilePath )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Control.Exception             as E
import           Control.Monad                  ( unless )
import           Data.Typeable                  ( Typeable )
import qualified Data.Aeson.Encode.Pretty      as Aeson
import qualified Data.ByteString.Lazy          as BL
import           Control.DeepSeq                ( deepseq )

group :: TestGroupProps -> [TestCase] -> TestGroup
group = TestGroup

testSuite :: [TestGroup] -> TestSuite
testSuite = TestSuite

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual left right =
    unless (left == right) $ leftStr `deepseq` rightStr `deepseq` E.throw
        (ExpectedButGot leftStr rightStr)
  where
    leftStr  = show left
    rightStr = show right

type Points = Int
type TimeoutSecs = Int

data ExpectedButGot = ExpectedButGot String String
    deriving (Eq, Show, Typeable, Generic)
    deriving anyclass (FromJSON, ToJSON, E.Exception)

type TestCase = IO ()

data TestGroupProps = TestGroupProps
    { label :: String
    , pointsPerTest :: !Points
    , penalty :: !Points
    , maximal :: !Points
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestGroup = TestGroup
    { testGroupProps :: TestGroupProps
    , testCases :: [TestCase]
    }

newtype TestSuite = TestSuite
    { testSuiteGroups :: [TestGroup]}

data TestCaseResult
    = TestCaseResultOk
    | TestCaseResultExpectedButGot ExpectedButGot
    | TestCaseResultException String
    | TestCaseResultTimeout
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestGroupResults = TestGroupResults
    { testCaseResults :: [TestCaseResult]
    , testGroupPoints :: Points
    , testGroupResultProps :: TestGroupProps
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

runTestCase :: TimeoutSecs -> TestCase -> IO TestCaseResult
runTestCase timeoutSecs testCase = fromMaybe TestCaseResultTimeout <$> timeout
    (timeoutSecs * 1000 * 1000)
    (           (testCase >> return TestCaseResultOk)
    `E.catches` [ E.Handler (return . TestCaseResultExpectedButGot)
                -- Re-throw AsyncException
                , E.Handler (throw :: E.AsyncException -> IO a)
                , E.Handler
                    (\(e :: E.SomeException) ->
                        return $ TestCaseResultException $ show e
                    )
                ]
    )

runTestGroup :: TimeoutSecs -> TestGroup -> IO TestGroupResults
runTestGroup timeoutSecs TestGroup {..} = do
    testCaseResults <- mapM (runTestCase timeoutSecs) testCases
    let testGroupPoints = getTestGroupPoints testGroupProps testCaseResults
    let testGroupResultProps = testGroupProps
    return TestGroupResults { .. }

runTestSuite :: TimeoutSecs -> TestSuite -> IO TestSuiteResults
runTestSuite timeoutSecs TestSuite {..} = do
    testGroupResults <- mapM (runTestGroup timeoutSecs) testSuiteGroups
    let testSuitePoints = sum . map testGroupPoints $ testGroupResults
    return TestSuiteResults { .. }
