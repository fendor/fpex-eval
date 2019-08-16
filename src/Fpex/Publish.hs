module Fpex.Publish where

import           System.Directory
import           Control.Monad.Extra                      ( whenM )

import           Fpex.Course.Types
import           Fpex.Eval.Types

-- | collect assignment of single student
publishTestResult :: Course -> TestSuite -> Student -> IO ()
publishTestResult course testSuite student = do

    let sourceFile = reportCollectFile course testSuite student
    let targetFile = reportPublishFile course testSuite student

    whenM (doesFileExist sourceFile) $ copyFile sourceFile targetFile
