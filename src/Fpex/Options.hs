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
    | Setup SetupCommand
    | Collect CollectCommand
    | Publish PublishCommand
    | Stats StatCommand
    deriving (Show)

data TestSuiteOptions = TestSuiteOptions
    { optionSubmissionId :: SubmissionId
    , optionTestSuiteSpecification :: FilePath
    }
    deriving (Show, Eq)

data SetupCommand = SetupCommand
    { useGroups :: Bool
    , prefix :: String
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

data StatCommand = StatCommand
    { statTestSuiteOptions :: TestSuiteOptions
    }
    deriving (Show, Eq)

options :: ParserInfo Options
options =
    let
        testSuiteParser = option
            str
            (long "test-suite" <> help "Test-suite specification (Haskell file)"
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
        setupParser =
            Setup
                <$> (   SetupCommand
                    <$> flag
                            True
                            False
                            (long "--use-groups" <> short 'g' <> help
                                "Course participants are working in groups"
                            )
                    <*> option
                            str
                            (  long "prefix"
                            <> help
                                   "Name prefix of participants. Applies to groups and students."
                            <> value "f"
                            )
                    )
        collectParser = Collect <$> (CollectCommand <$> testSuiteOptionParser)
        publishParser = Publish <$> (PublishCommand <$> testSuiteOptionParser)
        statParser    = Stats <$> (StatCommand <$> testSuiteOptionParser)

        commandParser = hsubparser
            (  command "setup"   (info setupParser fullDesc)
            <> command "collect" (info collectParser fullDesc)
            <> command "grade"   (info gradeParser fullDesc)
            <> command "publish" (info publishParser fullDesc)
            <> command "stats"   (info statParser fullDesc)
            )

        courseOption = optional $ option str (long "course")
        studentOption = optional $ Student <$> option str (long "student")

        parser = Options <$> commandParser <*> courseOption <*> studentOption
    in
        info (helper <*> parser) fullDesc
