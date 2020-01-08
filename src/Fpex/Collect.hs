module Fpex.Collect where

import           System.Directory
import           System.IO
import           Data.List
import           Control.Monad.Extra                      ( whenM )

import           Fpex.Course.Types
import           Fpex.Eval.Types

-- | collect assignment of single student
collectAssignment :: SubmissionId -> Course -> TestSuite -> Student -> IO ()
collectAssignment sid course testSuite student = do

    let sourceFile = studentSourceFile course testSuite student
    let targetDir  = assignmentCollectDir sid course testSuite
    let targetFile = assignmentCollectFile sid course testSuite student

    whenM (doesFileExist sourceFile) $ do
        whenM (isInfixOf "unsafePerformIO" <$> readFile sourceFile) $
            hPutStrLn stderr $ "Warning: `unsafePerformIO` in submission " <> sourceFile

        createDirectoryIfMissing False targetDir
        whenM (doesFileExist sourceFile) $ copyFile sourceFile targetFile
