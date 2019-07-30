module Fpex.Types where

import Data.Text (Text)

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

gradedPoints :: TestCase -> TestCaseResult -> Int
gradedPoints _ TestCaseCompilefail = 0
gradedPoints _ TestCaseNotSubmitted = 0
gradedPoints TestCase { expectedOutput, maxPoints } (TestCaseRun TestRun { actualOutput })
    = if expectedOutput == actualOutput then maxPoints else 0

newtype TestReport = TestReport
    { assignmentPoints :: [(TestCase, TestCaseResult)]
    } deriving (Eq, Show)

newtype TestSuite = TestSuite [TestCase] deriving (Eq, Show)

data TestCase = TestCase
    { query :: Text
    , expectedOutput :: Text
    , maxPoints :: Int
    } deriving (Eq, Show)
