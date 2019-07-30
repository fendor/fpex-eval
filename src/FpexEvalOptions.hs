module FpexEvalOptions where

import           Options.Applicative

newtype FpexEvalOptions = FpexEvalOptions
    { fpexEvalCommand :: FpexEvalCommand
    }
    deriving (Show)

data FpexEvalCommand = CommandGrade
    { student :: String
    , testSuiteFile :: String
    }
    deriving (Show)

fpexEvalOptions :: ParserInfo FpexEvalOptions
fpexEvalOptions =
    let gradeParser =
                FpexEvalOptions
                    <$> (CommandGrade <$> option str (long "student") <*> option
                            str
                            (long "test-suite")
                        )
        parser = hsubparser $ command "grade" (info gradeParser fullDesc)
    in  info (parser <**> helper) fullDesc
