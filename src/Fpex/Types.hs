module Fpex.Types where

newtype Student = Student
    { getId :: String
    }
    deriving (Ord, Eq, Show)

newtype TestRun = TestRun
    { actualOutput :: String
    }

data TestCaseResult
    = TestCaseRun TestRun
    | TestCaseCompilefail

gradedPoints :: TestCase -> TestCaseResult -> Int
gradedPoints _ TestCaseCompilefail = 0
gradedPoints TestCase { expectedOutput, maxPoints } (TestCaseRun TestRun { actualOutput })
    = if expectedOutput == actualOutput then maxPoints else 0

newtype TestReport = TestReport
    { assignmentPoints :: [(TestCase, TestCaseResult)]
    }

newtype TestSuite = TestSuite [TestCase]

data TestCase = TestCase
    { query :: String
    , expectedOutput :: String
    , maxPoints :: Int
    }
