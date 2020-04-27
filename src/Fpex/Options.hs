module Fpex.Options where

import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Eval.Types
import Options.Applicative

data Options
  = Options
      { optionCommand :: OptionCommand,
        optionCourseFile :: Maybe FilePath,
        optionStudent :: Maybe Student
        -- , optionSubmissionId :: SubmissionId
      }
  deriving (Show)

data OptionCommand
  = Setup SetupCommand
  | Lc TestSuiteOptions LifeCycle
  deriving (Show)

data LifeCycle
  = Collect CollectCommand
  | Grade GradeCommand
  | Publish PublishCommand
  | Stats StatCommand
  | SetTestSuite SetTestSuiteCommand
  | DiffResults DiffResultsCommand
  deriving (Show)

data TestSuiteOptions
  = TestSuiteOptions
      { optionSubmissionId :: SubmissionId,
        optionTestSuiteName :: T.Text
      }
  deriving (Show, Eq)

data SetTestSuiteCommand
  = SetTestSuiteCommand
      { setTestSuiteSpecification :: FilePath
      }
  deriving (Show, Eq)

data SetupCommand
  = SetupCommand
      { setupCourseRootDir :: Maybe FilePath,
        setupUserRegex :: String
      }
  deriving (Show, Eq)

data GradeCommand
  = GradeCommand
      { -- | Time in seconds each test may at most run before abort.
        testTimeout :: Timeout
      }
  deriving (Show, Eq)

data CollectCommand = CollectCommand
  deriving (Show, Eq)

data PublishCommand = PublishCommand
  deriving (Show, Eq)

data StatCommand
  = StatCommand
      { statOutputKind :: StatCommandOutputKind
      }
  deriving (Show, Eq)

data StatCommandOutputKind
  = StatsOutputCsv
  | StatsOutputGrades
  deriving (Show, Eq)

data DiffResultsCommand
  = DiffResultsCommand
      { diffResultSid :: SubmissionId
      }
  deriving (Show, Eq)

options :: ParserInfo Options
options =
  let testSuiteParser =
        option
          str
          ( long "test-suite" <> action "file" <> help "Test-suite specification (Haskell file)"
          )
      testSuiteNameParser =
        option
          str
          ( long "name" <> help "Name of the test-suite. If empty, takes name of the test-suite."
          )
      submissionIdParser =
        SubmissionId
          <$> option
            auto
            ( long "submission-id" <> short 'n'
                <> help
                  ( "Id of the submission."
                      <> " Can be used to collect, grade and publish an assignment multiple times."
                      <> " Submission id is added as a suffix to the submission folder."
                  )
            )
      oldSubmissionIdParser = SubmissionId <$> option auto (long "old" <> short 'o' <> help "Old Submission Id to compare the current submission to.")
      testSuiteOptionParser =
        TestSuiteOptions <$> submissionIdParser <*> testSuiteNameParser
      gradeParser =
        Grade
          <$> ( GradeCommand
                  <$> ( Timeout
                          <$> option
                            auto
                            ( long "timeout"
                                <> short 't'
                                <> help
                                  "Amount of time a test may run before receiving a timeout"
                                <> value 5.0
                            )
                      )
              )
      setupParser =
        Setup
          <$> ( SetupCommand
                  <$> optional
                    ( option
                        str
                        ( long "course-dir"
                            <> help
                              ( "Root directory of the course. It is"
                                  <> " expected that the name of the participants"
                                  <> " are described by the flag '--user-regex'"
                              )
                        )
                    )
                  <*> option
                    str
                    ( long "user-regex"
                        <> help
                          "Regex to use to find course participants"
                        <> value "f[0-9]{8}"
                    )
              )
      collectParser = Collect <$> pure CollectCommand
      publishParser = Publish <$> pure PublishCommand
      setTestSuiteParser = SetTestSuite <$> (SetTestSuiteCommand <$> testSuiteParser)
      statParser =
        Stats
          <$> ( StatCommand
                  <$> ( flag' StatsOutputCsv (long "csv")
                          <|> flag' StatsOutputGrades (long "grades")
                      )
              )
      diffResultParser = DiffResults <$> (DiffResultsCommand <$> oldSubmissionIdParser)
      withOptions lcParser = Lc <$> testSuiteOptionParser <*> lcParser
      commandParser =
        hsubparser
          ( command "setup" (info setupParser fullDesc)
              <> command "collect" (info (withOptions collectParser) fullDesc)
              <> command "grade" (info (withOptions gradeParser) fullDesc)
              <> command "publish" (info (withOptions publishParser) fullDesc)
              <> command "stats" (info (withOptions statParser) fullDesc)
              <> command "set-tests" (info (withOptions setTestSuiteParser) fullDesc)
              <> command "diff" (info (withOptions diffResultParser) fullDesc)
          )
      courseOption = optional $ option str (long "course")
      studentOption = optional $ Student <$> option str (long "student")
      parser = Options <$> commandParser <*> courseOption <*> studentOption
   in info (helper <*> parser) fullDesc
