module Fpex.EvalMain where

import           Options.Applicative
import           Fpex.EvalOptions
import           Fpex.Types
import           Control.Monad.Extra            ( whenJust )

defaultMain :: IO ()
defaultMain = execParser fpexEvalOptions >>= print

evalStudent :: Monad m => TestSuite -> Student -> m TestReport
evalStudent _ _ = undefined

studentFile :: Student -> Maybe FilePath
studentFile _ = undefined

generateReport :: Student -> TestReport -> FilePath -> IO ()
generateReport _ _ _ = undefined

grade :: TestSuite -> Student -> IO ()
grade testsuite student = do
    report <- evalStudent testsuite student
    let fileMay = studentFile student
    whenJust fileMay $ \fp -> generateReport student report fp