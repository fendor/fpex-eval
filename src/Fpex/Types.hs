module Fpex.Types where

import           Data.Text                      ( Text )

newtype OkTest = OkTest { getOkTest :: Int }
    deriving (Show)
    deriving newtype (Eq, Num, Ord)

newtype FailedTest = FailedTest { getFailedTest :: Int }
    deriving (Show)
    deriving newtype (Eq, Num, Ord)

newtype TimedOutTest = TimedOutTest { getTimedOutTest :: Int }
    deriving (Show)
    deriving newtype (Eq, Num, Ord)

newtype NotSubmittedTest = NotSubmittedTest { getNotSubmittedTest :: Int }
    deriving (Show)
    deriving newtype (Eq, Num, Ord)

newtype CompileFailTest = CompileFailTest { getCompileFailTest :: Int }
    deriving (Show)
    deriving newtype (Eq, Num, Ord)

data TestSummary =
    TestSummary
        { okTest :: OkTest
        , failedTest :: FailedTest
        , timedOutTest :: TimedOutTest
        , notSubmittedTest :: NotSubmittedTest
        , compileFailTest :: CompileFailTest
        } deriving (Show, Eq)

instance Semigroup TestSummary where
    t1 <> t2 = TestSummary
        { okTest           = okTest t1 + okTest t2
        , failedTest       = failedTest t1 + failedTest t2
        , timedOutTest     = timedOutTest t1 + timedOutTest t2
        , notSubmittedTest = notSubmittedTest t1 + notSubmittedTest t2
        , compileFailTest  = compileFailTest t1 + compileFailTest t2
        }

instance Monoid TestSummary where
    mempty = TestSummary { okTest           = 0
                         , failedTest       = 0
                         , timedOutTest     = 0
                         , notSubmittedTest = 0
                         , compileFailTest  = 0
                         }

testOk :: TestSummary
testOk = mempty { okTest = 1 }

testFailed :: TestSummary
testFailed = mempty { failedTest = 1 }

testTimeOut :: TestSummary
testTimeOut = mempty { timedOutTest = 1 }

testNotSubmitted :: TestSummary
testNotSubmitted = mempty { notSubmittedTest = 1 }

testCompileFail :: TestSummary
testCompileFail = mempty { compileFailTest = 1 }

newtype Points = Points { getPoints :: Int }
    deriving (Show)
    deriving newtype ( Eq, Num, Ord)

newtype Student = Student
    { matrNr :: Text
    }
    deriving (Ord, Eq, Show)

newtype TestRun = TestRun
    { actualOutput :: Text
    }
    deriving (Eq, Show)

data TestCaseResult
    = TestCaseRun TestRun
    | TestCaseCompilefail
    | TestCaseNotSubmitted
    | TestCaseTimeout
    deriving (Eq, Show)

data TestGroup a = TestGroup
    { label :: Text
    , pointsPerTest :: Points
    , penalty :: Points
    , group :: [a]
    }
    deriving (Eq, Show)

newtype TestReport = TestReport
    { assignmentPoints :: [TestGroup (TestCase, TestCaseResult)]
    } deriving (Eq, Show)

newtype TestSuite = TestSuite [TestGroup TestCase] deriving (Eq, Show)

data TestCase = TestCase
    { query :: Text
    , expectedOutput :: Text
    } deriving (Eq, Show)
