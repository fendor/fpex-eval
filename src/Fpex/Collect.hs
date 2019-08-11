module Fpex.Collect where

import           System.Directory
import           Control.Monad.Extra            ( whenM )

import           Fpex.Course.Types
import           Fpex.Eval.Types

-- | collect assignment of single student
collectAssignment :: Course -> TestSuite -> Student -> IO ()
collectAssignment course testSuite student = do

    let sourceFile = studentSourceFile course testSuite student
    let targetDir  = assignmentCollectDir course testSuite
    let targetFile = assignmentCollectFile course testSuite student

    createDirectoryIfMissing False $ targetDir
    whenM (doesFileExist sourceFile) $ copyFile sourceFile targetFile
