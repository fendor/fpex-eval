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
    , groupName :: T.Text
    , groupPrefix :: T.Text
    }
    deriving (Show)

options :: ParserInfo Options
options =
    let gradeParser =
                Eval
                    <$> (   CommandGrade
                        <$> ((Student . T.pack) <$> option str (long "student"))
                        <*> (option str (long "test-suite"))
                        )
        userParser = undefined
        parser     = hsubparser $ command "grade" (info gradeParser fullDesc)
    in  info (parser <**> helper) fullDesc
