module Fpex.Options where

import           Options.Applicative
import qualified Data.Text                     as T
import           Fpex.Course.Types
import           Fpex.Eval.Types

data Options = Options
    { optionCommand :: OptionCommand
    , optionCourseFile :: FilePath
    , optionStudent :: Maybe Student
    }

data OptionCommand
    = Grade CommandGrade
    | Setup
    | Collect CollectCommand
    | Publish PublishCommand
    deriving (Show)

data CommandGrade = CommandGrade
    { testSuiteSpec :: TestSuiteSpecification
    -- ^ Test-suite to grade.
    , gradeRunner :: GradeRunner
    -- ^ Which runner to use for executing the tests, e.g. Hugs or Ghci.
    , testTimeout :: Timeout
    -- ^ Time in seconds each test may at most run before abort.
    }
    deriving (Show, Eq)

data TestSuiteSpecification
    = Legacy FilePath
    -- ^ Legacy specification of the test-suite
    | Json FilePath
    -- ^ Modern json specification of the test-suite
    deriving (Show, Eq, Read)

data CollectCommand = CollectCommand
    { collectTestSuiteFile :: FilePath
    }
    deriving (Show)

data PublishCommand = PublishCommand
    { publishTestSuiteFile :: FilePath
    }
    deriving (Show)

options :: ParserInfo Options
options =
    let
        gradeParser =
            Grade
                <$> (   CommandGrade
                    <$> (   Json
                        <$> option
                                str
                                (  long "test-suite"
                                <> help
                                       "Test-suite specification using the json format"
                                )
                        <|> Legacy
                        <$> option
                                str
                                (  long "test-suite-legacy"
                                <> help
                                       "Test-suite specification using the legacy format"
                                )
                        )
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

        collectParser =
            Collect <$> (CollectCommand <$> option str (long "test-suite"))
        publishParser =
            Publish <$> (PublishCommand <$> option str (long "test-suite"))

        commandParser = hsubparser
            (  command "setup"   (info (pure Setup) fullDesc)
            <> command "collect" (info collectParser fullDesc)
            <> command "grade"   (info gradeParser fullDesc)
            <> command "publish" (info publishParser fullDesc)
            )

        courseOption = option str (long "course")
        studentOption =
            optional $ Student . T.pack <$> option str (long "student")
        parser = Options <$> commandParser <*> courseOption <*> studentOption
    in
        info (helper <*> parser) fullDesc
