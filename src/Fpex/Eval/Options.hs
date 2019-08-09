module Fpex.Eval.Options where

import           Options.Applicative
import qualified Data.Text                     as T
import           Fpex.User.Types
import           Fpex.Eval.Types

data Options
    = Eval EvalCommand
    | User UserManagementCommand

    deriving (Show)

data EvalCommand = CommandGrade
    { student :: Student
    , testSuiteFile :: FilePath
    }
    deriving (Show)

data UserManagementCommand = UserManagementCommand
    { username :: Username
    , userGroup :: Group
    }
    deriving (Show)

options :: ParserInfo Options
options =
    let
        gradeParser =
            Eval
                <$> (   CommandGrade
                    <$> (Student . T.pack <$> option str (long "student"))
                    <*> option str (long "test-suite")
                    )
        buildUserManagement user group prefix = UserManagementCommand
            user
            (Group { getGroup = group, getPrefix = prefix })
        userParser =
            User
                <$> (   buildUserManagement
                    <$> (Username . T.pack <$> option str (long "username"))
                    <*> (T.pack <$> option str (long "group"))
                    <*> (   fmap T.pack
                        <$> optional (option str (long "home-prefix"))
                        )
                    )
        parser = hsubparser
            (  command "grade" (info gradeParser fullDesc)
            <> command "user"  (info userParser fullDesc)
            )
    in
        info (parser <**> helper) fullDesc
