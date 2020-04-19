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
    deriving (Show)

data OptionCommand
    = Grade GradeCommand
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
    { setupCourseRootDir :: Maybe FilePath 
    , setupUserRegex :: String
    }
    deriving (Show, Eq)

data GradeCommand = GradeCommand
    { gradeTestSuiteOptions :: TestSuiteOptions
    -- ^ Test-suite to grade.
    , testTimeout :: Timeout
    -- ^ Time in seconds each test may at most run before abort.
    }
    deriving (Show, Eq)

data CollectCommand = CollectCommand
    { collectTestSuiteOptions :: TestSuiteOptions
    }
    deriving (Show, Eq)

data PublishCommand = PublishCommand
    { publishTestSuiteOptions :: TestSuiteOptions
    }
    deriving (Show, Eq)

data StatCommand = StatCommand
    { statTestSuiteOptions :: TestSuiteOptions
    , statOutputKind :: StatCommandOutputKind
    }
    deriving (Show, Eq)

data StatCommandOutputKind
    = StatsOutputCsv
    | StatsOutputGrades
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
                <$> (   GradeCommand
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
                    <$> optional 
                            ( option str 
                                (  long "course-dir"
                                <> help 
                                    ( "Root directory of the course. It is"
                                    <> " expected that the name of the participants"
                                    <> " are described by the flag '--user-regex'"
                                    )
                                )
                            )
                    <*> option
                        str
                            (  long "user-regex"
                            <> help
                                    "Regex to use to find course participants"
                            <> value "f[0-9]{8}"
                            )
                    )
        collectParser = Collect <$> (CollectCommand <$> testSuiteOptionParser)
        publishParser = Publish <$> (PublishCommand <$> testSuiteOptionParser)
        statParser =
            Stats
                <$> (   StatCommand
                    <$> testSuiteOptionParser
                    <*> (   flag' StatsOutputCsv    (long "csv")
                        <|> flag' StatsOutputGrades (long "grades")
                        )
                    )

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
