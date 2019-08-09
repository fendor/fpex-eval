module Fpex.Eval.Options where

import           Options.Applicative
import           Data.Text                                ( Text )
import qualified Data.Text                                as T

newtype FpexEvalOptions = FpexEvalOptions
    { fpexEvalCommand :: FpexEvalCommand
    }
    deriving (Show)

data FpexEvalCommand = CommandGrade
    { student :: Text
    , testSuiteFile :: FilePath
    }
    deriving (Show)

fpexEvalOptions :: ParserInfo FpexEvalOptions
fpexEvalOptions =
    let gradeParser =
                FpexEvalOptions
                <$> (   CommandGrade
                    <$> (T.pack <$> option str (long "student"))
                    <*> (option str (long "test-suite"))
                    )
        parser = hsubparser $ command "grade" (info gradeParser fullDesc)
    in  info (parser <**> helper) fullDesc
