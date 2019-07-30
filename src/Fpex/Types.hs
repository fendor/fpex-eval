module Fpex.Types where

import           Data.Text                      ( Text )

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
    deriving (Eq, Show)


newtype TestReport = TestReport
    { assignmentPoints :: [(TestCase, TestCaseResult)]
    } deriving (Eq, Show)

newtype TestSuite = TestSuite [TestCase] deriving (Eq, Show)

data TestCase = TestCase
    { query :: Text
    , expectedOutput :: Text
    , maxPoints :: Int
    } deriving (Eq, Show)
