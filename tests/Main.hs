module Main where

import qualified Test.Tasty
import           Test.Tasty.Hspec
import           Fpex.Eval.Main
import           Fpex.Eval.Summary
import           Fpex.Eval.Types
import           Fpex.Eval.Effect
import           Fpex.Course.Types
import qualified Data.Text                     as T
import           Control.Monad                  ( forM_ )

import           Polysemy                       ( runM )

-- other tests
import qualified Course

main :: IO ()
main = do
  test <- testSpec "fpex-eval" $ do
    spec
    Course.spec
  Test.Tasty.defaultMain test

data Submission = Submission
    { student :: Student
    , testResults :: [[TestCaseResult]]
    , testSummary :: TestSummary
    , points :: Points
    }

testSuiteSimple :: TestSuite
testSuiteSimple = TestSuite "assignment1" testCasesSimple

fibTestGroup :: TestGroup TestCase
fibTestGroup = TestGroup
  { label         = "Fibonacci tests"
  , pointsPerTest = Points 5
  , penalty       = Points 0
  , group         = [ TestCase "fib 0" "1"
                    , TestCase "fib 1" "1"
                    , TestCase "fib 2" "2"
                    , TestCase "fib 3" "3"
                    , TestCase "fib 4" "5"
                    , TestCase "fib 5" "8"
                    ]
  }

testCourse :: Course
testCourse = Course
  { courseName       = "course1"
  , courseRootDir    = "testdata/course1"
  , courseStudents   = [ Student "1234567"
                       , Student "1000000"
                       , Student "1113330"
                       , Student "1711000"
                       , Student "1831000"
                       ]
  , courseGroups     = []
  , courseUserPrefix = "c1-"
  }

factorialTestGroup :: TestGroup TestCase
factorialTestGroup = TestGroup
  { label         = "Factorial tests"
  , pointsPerTest = Points 10
  , penalty       = Points 0
  , group         = [ TestCase "factorial 3"  "6"
                    , TestCase "factorial 0"  "1"
                    , TestCase "factorial 10" "3628800"
                    ]
  }

testCasesSimple :: [TestGroup TestCase]
testCasesSimple = [fibTestGroup, factorialTestGroup]

submissionsSimple :: [Submission]
submissionsSimple =
  [ Submission -- ^ Everything correct
    { student     = Student "1234567"
    , testResults = [ [ TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "2"
                      , TestCaseRun $ TestRun "3"
                      , TestCaseRun $ TestRun "5"
                      , TestCaseRun $ TestRun "8"
                      ]
                    , [ TestCaseRun $ TestRun "6"
                      , TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "3628800"
                      ]
                    ]
    , testSummary = mempty { okTest = 9 }
    , points      = 60
    }
  , Submission -- ^ Fibonacci sequence incorrectly defined as f_(n+2) = f_n + f_n
        -- Also, no submission for the function "factorial"
    { student     = Student "1000000"
    , testResults = [ [ TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "2"
                      , TestCaseRun $ TestRun "2"
                      , TestCaseRun $ TestRun "4"
                      , TestCaseRun $ TestRun "4"
                      ]
                    , [ TestCaseCompilefail
                      , TestCaseCompilefail
                      , TestCaseCompilefail
                      ]
                    ]
    , testSummary = mempty { okTest = 3, failedTest = 6, compileFailTest = 3 }
    , points      = 15
    }
  , Submission -- ^ Fibonacci sequence incorrectly defined as f_(n+2) = f_n + f_n
    { student     = Student "1711000"
    , testResults = [ [ TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "2"
                      , TestCaseRun $ TestRun "2"
                      , TestCaseRun $ TestRun "4"
                      , TestCaseRun $ TestRun "4"
                      ]
                    , [ TestCaseRun $ TestRun "6"
                      , TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "3628800"
                      ]
                    ]
    , testSummary = mempty { okTest = 6, failedTest = 3, compileFailTest = 0 }
    , points      = 45
    }
  , Submission -- ^ Submission contains a typo. Thus, everything fails
    { student     = Student "1831000"
    , testResults = [ [ TestCaseCompilefail
                      , TestCaseCompilefail
                      , TestCaseCompilefail
                      , TestCaseCompilefail
                      , TestCaseCompilefail
                      , TestCaseCompilefail
                      ]
                    , [ TestCaseCompilefail
                      , TestCaseCompilefail
                      , TestCaseCompilefail
                      ]
                    ]
    , testSummary = mempty { failedTest = 9, compileFailTest = 9 }
    , points      = 0
    }
  , Submission -- ^ No submission for that matriculation number
    { student     = Student "1456000"
    , testResults = [ [ TestCaseNotSubmitted
                      , TestCaseNotSubmitted
                      , TestCaseNotSubmitted
                      , TestCaseNotSubmitted
                      , TestCaseNotSubmitted
                      , TestCaseNotSubmitted
                      ]
                    , [ TestCaseNotSubmitted
                      , TestCaseNotSubmitted
                      , TestCaseNotSubmitted
                      ]
                    ]
    , testSummary = mempty { failedTest = 9, notSubmittedTest = 9 }
    , points      = 0
    }
  , Submission -- ^ This case receives a timeout
    { student     = Student "1113330"
    , testResults = [ [ TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "1"
                      , TestCaseTimeout
                      , TestCaseTimeout
                      , TestCaseTimeout
                      , TestCaseTimeout
                      ]
                    , [ TestCaseRun $ TestRun "6"
                      , TestCaseRun $ TestRun "1"
                      , TestCaseRun $ TestRun "3628800"
                      ]
                    ]
    , testSummary = mempty { okTest = 5, failedTest = 4, timedOutTest = 4 }
    , points      = 40
    }
  ]

spec :: Spec
spec =
  parallel
    $ describe "evaluate student, all points"
    $ forM_ submissionsSimple
    $ \submission ->
        describe
            ("submission of student" <> T.unpack (matrNr $ student submission))
          $ do
              it "evaluate tests" $ do
                report <- runEvalStudent testCourse
                                         testSuiteSimple
                                         (student submission)
                map (map snd . group) (assignmentPoints report)
                  `shouldBe` testResults submission

              it "verify report" $ do
                report <- runEvalStudent testCourse
                                         testSuiteSimple
                                         (student submission)
                gradeReport report `shouldBe` testSummary submission

              it "computed points" $ do
                report <- runEvalStudent testCourse
                                         testSuiteSimple
                                         (student submission)
                scoreReport report `shouldBe` points submission

runEvalStudent :: Course -> TestSuite -> Student -> IO TestReport
runEvalStudent course suite submission =
  runM . runGrade . runStudentData $ evalStudent course suite submission
