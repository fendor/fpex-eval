{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}


module TestSpec where

import qualified Data.Generics.Uniplate.Operations
                                               as Uniplate
import           Data.Generics.Uniplate.Data    ( )
import           Data.Data                      ( Data )
import           Data.Maybe                     ( fromMaybe )
import           System.Timeout                 ( timeout )
import           System.IO                      ( FilePath )
import           System.Clock
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

import           Language.Haskell.TH           as TH
import qualified Language.Haskell.TH.Syntax    as TH
import           Language.Haskell.TH.Quote

group :: TestGroupProps -> [TestCase] -> TestGroup
group = TestGroup

testSuite :: [TestGroup] -> TestSuite
testSuite = TestSuite

testcase :: Q Exp -> Q Exp
testcase e = do
    let unbound :: Exp -> Bool
        unbound m = not $ null [ () | TH.UnboundVarE{} <- Uniplate.universe m ]

    expr <- runQ e

    let simplifyName :: Name -> Name
        simplifyName (TH.Name occ _) = TH.Name occ TH.NameS
        simplifyNames :: Data a => a -> a
        simplifyNames = Uniplate.transformBi simplifyName
    let prettyExpr = LitE $ StringL $ pprint (simplifyNames expr)

    generatedExpr <- if unbound expr
        then [e|evaluate (throw NotSubmitted)|]
        else pure expr

    tupE [pure prettyExpr, pure generatedExpr]


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

data NotSubmitted = NotSubmitted
    deriving (Eq, Show, Typeable, Generic)
    deriving anyclass (FromJSON, ToJSON, E.Exception)

type TestCase = (String, IO ())

data TestGroupProps = TestGroupProps
    { label :: String
    , pointsPerTest :: !Points
    , penalty :: !Points
    , upperBound :: !Points
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
    | TestCaseResultNotSubmitted
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestCaseReport = TestCaseReport
    { testCaseReportLabel :: String
    , testCaseReportResult :: TestCaseResult
    , testCaseReportTimeNs :: Integer
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestGroupResults = TestGroupResults
    { testGroupReports :: [TestCaseReport]
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
    max 0 . min upperBound . sum . map (getTestCasePoints props)


-- runners

runTestCase :: TimeoutSecs -> TestCase -> IO TestCaseReport
runTestCase timeoutSecs (testCaseReportLabel, testCaseAction) = do
    before               <- getTime Monotonic
    testCaseReportResult <- fromMaybe TestCaseResultTimeout <$> timeout
        (timeoutSecs * 1000 * 1000)
        (           (testCaseAction >> return TestCaseResultOk)
        `E.catches` [ E.Handler (return . TestCaseResultExpectedButGot)
                    , E.Handler
                        (\(_ :: NotSubmitted) ->
                            return TestCaseResultNotSubmitted
                        )
                    -- Re-throw AsyncException
                    , E.Handler (throw :: E.AsyncException -> IO a)
                    , E.Handler
                        (\(e :: E.SomeException) ->
                            return $ TestCaseResultException $ show e
                        )
                    ]
        )
    now <- getTime Monotonic
    let diff                 = diffTimeSpec now before
    let testCaseReportTimeNs = timeSpecToNs diff
    return TestCaseReport { .. }
  where
    timeSpecToNs :: TimeSpec -> Integer
    timeSpecToNs spec =
        fromIntegral (sec spec) * 10 ^ 9 + fromIntegral (nsec spec)

runTestGroup :: TimeoutSecs -> TestGroup -> IO TestGroupResults
runTestGroup timeoutSecs TestGroup {..} = do
    testGroupReports <- mapM (runTestCase timeoutSecs) testCases
    let testGroupPoints = getTestGroupPoints testGroupProps
            $ map testCaseReportResult testGroupReports
    let testGroupResultProps = testGroupProps
    return TestGroupResults { .. }

runTestSuite :: TimeoutSecs -> TestSuite -> IO ()
runTestSuite timeoutSecs TestSuite {..} = do
    testGroupResults <- mapM (runTestGroup timeoutSecs) testSuiteGroups
    let testSuitePoints = sum . map testGroupPoints $ testGroupResults
    BL.writeFile "report.json" $ Aeson.encodePretty TestSuiteResults { .. }
