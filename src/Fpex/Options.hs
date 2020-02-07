module Fpex.Options where

import           Options.Applicative
import           Fpex.Course.Types
import           Fpex.Eval.Types

data Options = Options
    { optionCommand :: OptionCommand
    , optionCourseFile :: Maybe FilePath
    , optionStudent :: Maybe Student
    -- , optionSubmissionId :: SubmissionId
    }

data OptionCommand
    = Grade CommandGrade
    | Setup
    | Collect CollectCommand
    | Publish PublishCommand
    deriving (Show)

data TestSuiteOptions = TestSuiteOptions
    { optionSubmissionId :: SubmissionId
    , optionTestSuiteSpecification :: FilePath
    }
    deriving (Show, Eq)

data CommandGrade = CommandGrade
    { gradeTestSuiteOptions :: TestSuiteOptions
    -- ^ Test-suite to grade.
    , testTimeout :: Timeout
    -- ^ Time in seconds each test may at most run before abort.
    }
    deriving (Show, Eq)

data CollectCommand = CollectCommand
    { collectTestSuiteOptions :: TestSuiteOptions
    }
    deriving (Show)

data PublishCommand = PublishCommand
    { publishTestSuiteOptions :: TestSuiteOptions
    }
    deriving (Show)

options :: ParserInfo Options
options =
    let
        testSuiteParser = option
            str
            (  long "test-suite"
            <> help "Test-suite specification (Haskell file)"
            )

        submissionIdParser = SubmissionId <$> option
            auto
            (long "submission-id" <> short 'n' <> help
                ("Id of the submission."
                <> " Can be used to collect, grade and publish an assignment multiple times."
                <> " Submission id is added as a suffix to the submission folder."
                )
            )

        testSuiteOptionParser =
            TestSuiteOptions <$> submissionIdParser <*> testSuiteParser

        gradeParser =
            Grade
                <$> (   CommandGrade
                    <$> testSuiteOptionParser
                    <*> (Timeout <$> option
                            auto
                            (  long "timeout"
                            <> short 't'
                            <> help
                                   "Amount of time a test may run before receiving a timeout"
                            <> value 5.0
                            )
                        )
                    )

        collectParser = Collect <$> (CollectCommand <$> testSuiteOptionParser)
        publishParser = Publish <$> (PublishCommand <$> testSuiteOptionParser)

        commandParser = hsubparser
            (  command "setup"      (info (pure Setup) fullDesc)
            <> command "collect"    (info collectParser fullDesc)
            <> command "grade"      (info gradeParser fullDesc)
            <> command "publish"    (info publishParser fullDesc)
            )

        courseOption = optional $ option str (long "course")
        studentOption = optional $ Student <$> option str (long "student")

        parser = Options <$> commandParser <*> courseOption <*> studentOption
    in
        info (helper <*> parser) fullDesc
