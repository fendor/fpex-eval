module Fpex.Options where

import Data.Set (Set)
import qualified Data.Set as Set
import Fpex.Course.Types
import Options.Applicative

data Options = Options
  { optionCommand :: OptionCommand,
    optionCourseFile :: Maybe FilePath,
    optionStudent :: Set Student,
    optionSkipStudent :: Set Student
  }
  deriving (Show)

data OptionCommand
  = Setup SetupCommand
  | Lc TestSuiteOptions LifeCycle
  | FinalPoints FinalPointsCommand
  deriving (Show)

data FinalPointsCommand = FinalPointsCommand
  { finalPointsSubmissionIds :: [SubmissionId],
    finalPointsSubmissions :: [SubmissionName],
    finalPointsOutput :: FilePath,
    finalPointsFeedback :: FeedbackAction
  }
  deriving (Show, Eq, Ord)

data LifeCycle
  = Collect CollectCommand
  | Grade GradeCommand
  | Feedback FeedbackCommand
  | Stats StatCommand
  | DiffResults DiffResultsCommand
  | RecalculatePoints
  deriving (Show)

data TestSuiteOptions = TestSuiteOptions
  { optionSubmissionId :: SubmissionId,
    optionSubmissionName :: SubmissionName,
    optionStudentSubmission :: Maybe StudentSubmission
  }
  deriving (Show, Eq)

data SetTestSuiteCommand = SetTestSuiteCommand
  { setTestSuiteSpecification :: FilePath
  }
  deriving (Show, Eq)

data SetupCommand = SetupCommand
  { setupCourseRootDir :: Maybe FilePath,
    setupUserRegex :: String
  }
  deriving (Show, Eq)

data GradeCommand = GradeCommand
  { -- | Time in seconds each test may at most run before abort.
    testTimeout :: Timeout,
    gradeBaseDefinitions :: FilePath,
    -- | Set the test-suite for the grading cycle.
    gradeTestSuite :: Maybe TestSuitePath
  }
  deriving (Show, Eq)

data CollectCommand = CollectCommand
  deriving (Show, Eq)

data FeedbackCommand = FeedbackCommand
  { feedbackPublish :: FeedbackAction
  }
  deriving (Show, Eq)

data StatCommand = StatCommand
  { statOutputKind :: StatCommandOutputKind
  }
  deriving (Show, Eq)

data StatCommandOutputKind
  = StatsOutputCsv
  | StatsOutputGrades
  deriving (Show, Eq)

data DiffResultsCommand = DiffResultsCommand
  { diffResultSid :: SubmissionId
  }
  deriving (Show, Eq)

options :: ParserInfo Options
options =
  let testSuiteParser =
        TestSuitePath
          <$> strOption
            ( long "test-suite" <> action "file" <> help "Test-suite specification (Haskell file)"
            )

      studentSubmissionParser =
        StudentSubmission
          <$> strOption
            ( long "submission" <> action "file" <> help "Name of the submission"
            )
      submissionNameParser =
        SubmissionName
          <$> strOption
            ( long "name" <> help "Name of the test-suite."
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
      oldSubmissionIdParser =
        SubmissionId
          <$> option
            auto
            ( long "old" <> short 'o'
                <> help "Old Submission Id to compare the current submission to."
            )
      testSuiteOptionParser =
        TestSuiteOptions <$> submissionIdParser <*> submissionNameParser <*> optional studentSubmissionParser
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
                  <*> strOption
                    ( long "definitions"
                        <> action "file"
                        <> help
                          ( "Describes functions and datatypes under test."
                              <> " This is the base submission, used to evaluate all students."
                          )
                    )
                  <*> optional testSuiteParser
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
      publishParser =
        Feedback
          <$> ( FeedbackCommand
                  <$> flag
                    WriteFeedback
                    PublishFeedback
                    (long "publish" <> help "Publish the feedback")
              )
      statParser =
        Stats
          <$> ( StatCommand
                  <$> ( flag' StatsOutputCsv (long "csv")
                          <|> flag' StatsOutputGrades (long "grades")
                      )
              )
      diffResultParser = DiffResults <$> (DiffResultsCommand <$> oldSubmissionIdParser)
      finalPointsParser =
        FinalPoints
          <$> ( FinalPointsCommand <$> many submissionIdParser <*> many submissionNameParser
                  <*> option
                    str
                    ( long "output"
                        <> help
                          "Output directory for point results"
                    )
                  <*> flag
                    WriteFeedback
                    PublishFeedback
                    (long "publish" <> help "Publish the feedback")
              )
      withOptions lcParser = Lc <$> testSuiteOptionParser <*> lcParser
      commandParser =
        hsubparser
          ( ( command "setup" (info setupParser fullDesc)
                <> help "Setup a directory for grading student assignments"
            )
              <> ( command "collect" (info (withOptions collectParser) fullDesc)
                     <> help "Collect student assignments"
                 )
              <> command "grade" (info (withOptions gradeParser) fullDesc)
              <> command "feedback" (info (withOptions publishParser) fullDesc)
              <> command "stats" (info (withOptions statParser) fullDesc)
              <> command "diff" (info (withOptions diffResultParser) fullDesc)
              <> command "recalculate" (info (withOptions $ pure RecalculatePoints) fullDesc)
              <> command "final-points" (info finalPointsParser fullDesc)
          )
      courseOption = optional $ option str (long "course")
      studentOption = Student <$> option str (long "student" <> help "Execute the command only for the given student")
      skipStudentOption = Student <$> option str (long "skip-student" <> help "Skip the given student")
      parser =
        Options
          <$> commandParser
            <*> courseOption
            <*> (fmap Set.fromList $ many studentOption)
            <*> (fmap Set.fromList $ many skipStudentOption)
   in info (helper <*> parser) fullDesc
