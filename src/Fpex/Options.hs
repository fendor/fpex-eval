module Fpex.Options where

import           Options.Applicative
import qualified Data.Text                     as T
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
    | EdslGrade TestSuiteOptions
    | Setup
    | Collect CollectCommand
    | Publish PublishCommand
    deriving (Show)

data TestSuiteOptions = TestSuiteOptions
    { optionSubmissionId :: SubmissionId
    , optionTestSuiteSpecification :: TestSuiteSpecification
    }
    deriving (Show, Eq)

data TestSuiteSpecification
    = Legacy FilePath T.Text
    -- ^ Legacy specification of the test-suite.
    -- Also adds the assignment name since this information
    -- is not contained in the legacy specification.
    | Json FilePath
    -- ^ Modern json specification of the test-suite
    | EdslSpec FilePath
    -- ^ specification of the test-suite using the edsl format
    deriving (Show, Eq, Read)

data CommandGrade = CommandGrade
    { gradeTestSuiteOptions :: TestSuiteOptions
    -- ^ Test-suite to grade.
    , gradeRunner :: GradeRunner
    -- ^ Which runner to use for executing the tests, e.g. Hugs or Ghci.
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
        testSuiteParser =
            Json
                <$> option
                        str
                        (long "test-suite" <> help
                            "Test-suite specification using the json format"
                        )
                <|> Legacy
                <$> option
                        str
                        (long "legacy-test-suite" <> help
                            "Test-suite specification using the legacy format"
                        )
                <*> option
                        str
                        (  long "legacy-assignment-name"
                        <> help
                               "Assignment name. Required since this information is not contained in the legacy specification."
                        )
                <|> EdslSpec
                <$> option 
                        str
                        (long "edsl-test-suite" <> help
                            "Test-suite specification using the edsl format"
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

        edslGradeParser = EdslGrade <$> testSuiteOptionParser

        gradeParser =
            Grade
                <$> (   CommandGrade
                    <$> testSuiteOptionParser
                    <*> (   flag'
                              Hugs
                              (  long "hugs"
                              <> help "Grade students using 'Hugs'"
                              )
                        <|> flag'
                                Ghci
                                (  long "ghci"
                                <> help
                                       "Grade students using 'Ghci' (default)"
                                )
                        <|> flag'
                                SavedGhci
                                (  long "ghci-session"
                                <> help
                                       "Grade students using 'Ghci', reusing a Session if possible"
                                )
                        <|> pure Ghci
                        )
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
            (  command "setup"   (info (pure Setup) fullDesc)
            <> command "collect" (info collectParser fullDesc)
            <> command "grade"   (info gradeParser fullDesc)
            <> command "edsl-grade"   (info edslGradeParser fullDesc)
            <> command "publish" (info publishParser fullDesc)
            )

        courseOption = optional $ option str (long "course")
        studentOption = optional $ Student <$> option str (long "student")

        parser = Options <$> commandParser <*> courseOption <*> studentOption
    in
        info (helper <*> parser) fullDesc
