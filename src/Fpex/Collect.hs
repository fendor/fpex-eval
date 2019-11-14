module Fpex.Collect where

import           System.Directory
import           Control.Monad.Extra                      ( whenM )

import           Fpex.Course.Types
import           Fpex.Eval.Types

-- | collect assignment of single student
collectAssignment :: SubmissionId -> Course -> TestSuite -> Student -> IO ()
collectAssignment sid course testSuite student = do

    let sourceFile = studentSourceFile course testSuite student
    let targetDir  = assignmentCollectDir sid course testSuite
    let targetFile = assignmentCollectFile sid course testSuite student

    createDirectoryIfMissing False $ targetDir
    whenM (doesFileExist sourceFile) $ copyFile sourceFile targetFile
