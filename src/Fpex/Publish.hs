module Fpex.Publish where

import           System.Directory
import           Control.Monad.Extra                      ( whenM )

import           Fpex.Course.Types
import           Fpex.Eval.Types

-- | collect assignment of single student
publishTestResult :: SubmissionId -> Course -> String -> Student -> IO ()
publishTestResult sid course testSuite student = do
    return ()

    -- let sourceFile = reportCollectFile sid course testSuite student
    -- let targetFile = reportPublishFile sid course testSuite student

    -- whenM (doesFileExist sourceFile) $ copyFile sourceFile targetFile
