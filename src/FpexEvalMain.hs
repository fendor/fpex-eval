module FpexEvalMain where

import Options.Applicative

import FpexEvalOptions

fpexMain :: IO ()
fpexMain = execParser fpexEvalOptions >>= print
