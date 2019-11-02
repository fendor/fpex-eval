module Fpex.Options where

import           Options.Applicative
import qualified Data.Text                     as T
import           Fpex.Course.Types

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
    { testSuiteFile :: FilePath
    }
    deriving (Show)

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
                    <$> option str (long "test-suite")
                    )

        collectParser =
            Collect <$> (CollectCommand <$> option str (long "test-suite"))
        publishParser =
            Publish <$> (PublishCommand <$> option str (long "test-suite"))

        commandParser = hsubparser
            ( command "setup" (info (pure Setup) fullDesc)
            <> command "collect" (info collectParser fullDesc)
            <> command "grade" (info gradeParser fullDesc)
            <> command "publish" (info publishParser fullDesc)
            )

        courseOption = option str (long "course")
        studentOption = optional $ Student . T.pack <$> option str (long "student")
        parser = Options <$> commandParser <*> courseOption <*> studentOption
    in
        info (helper <*> parser) fullDesc
