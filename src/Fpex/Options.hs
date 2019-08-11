module Fpex.Options where

import           Options.Applicative
import qualified Data.Text                     as T
import           Fpex.User.Types
import           Fpex.Course.Types

data Options = Options
    { optionCommand :: OptionCommand
    , optionCourseFile :: FilePath
    , optionStudent :: Maybe Student
    }

data OptionCommand
    = Grade CommandGrade
    | User UserManagementCommand
    | Setup
    | Collect CollectCommand
    deriving (Show)

data CommandGrade = CommandGrade
    { testSuiteFile :: FilePath
    }
    deriving (Show)

data UserManagementCommand = UserManagementCommand
    { username :: Username
    , userGroup :: UserGroup
    }
    deriving (Show)

data CollectCommand = CollectCommand
    { collectTestSuiteFile :: FilePath
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
        buildUserManagement user group prefix = UserManagementCommand
            user
            (UserGroup { group, prefix })
        userParser =
            User
                <$> (   buildUserManagement
                    <$> (Username . T.pack <$> option str (long "username"))
                    <*> (T.pack <$> option str (long "group"))
                    <*> (   fmap T.pack
                        <$> optional (option str (long "home-prefix"))
                        )
                    )

        collectParser =
            Collect <$> (CollectCommand <$> option str (long "test-suite"))

        commandParser = hsubparser
            (  command "grade" (info gradeParser fullDesc)
            <> command "user"  (info userParser fullDesc)
            <> command "setup" (info (pure Setup) fullDesc)
            <> command "collect" (info collectParser fullDesc)
            )

        courseOption = option str (long "course")
        studentOption = optional $ Student . T.pack <$> option str (long "student")
        parser = Options <$> commandParser <*> courseOption <*> studentOption
    in
        info (helper <*> parser) fullDesc
