module Fpex.Collect where

import Control.Monad.Extra (whenM)
import qualified Data.ByteString as BS
import Fpex.Course.Types
import Fpex.Grade.Types
import System.Directory
import System.FilePath
import System.IO
import Fpex.Grade.ErrorStudent (errorStudent)

data FailureReason = NoSubmission
  deriving (Show, Eq, Read)

setTestSuite :: SubmissionId -> Assignment -> FilePath -> IO ()
setTestSuite sid suiteName testSuite = do
  let targetDir = assignmentCollectDir sid suiteName
  createDirectoryIfMissing True targetDir

  -- copy assignment main
  let testSuiteTarget = targetDir </> "Main.hs"
  putStrLn $ "copy " <> testSuite <> " to " <> targetDir
  copyFile testSuite testSuiteTarget

prepareSubmissionFolder :: SubmissionId -> Assignment -> IO ()
prepareSubmissionFolder sid suiteName = do
  let targetDir = assignmentCollectDir sid suiteName
  createDirectoryIfMissing True targetDir

collectSubmission :: SubmissionId -> Course -> Assignment -> Student -> IO (Either FailureReason FilePath)
collectSubmission sid course suiteName student = do
  let sourceFile = studentSourceFile course suiteName student
  let targetDir = assignmentCollectStudentDir sid suiteName student
  let targetFile = assignmentCollectStudentFile sid suiteName student
  -- create submission dir even if there is no submission
  createDirectoryIfMissing True targetDir

  -- copy submission file if it exists
  fileExists <- doesFileExist sourceFile
  if fileExists
    then do
      whenM (BS.isInfixOf "unsafePerformIO" <$> BS.readFile sourceFile) $
        hPutStrLn stderr $
          "Warning: `unsafePerformIO` in submission "
            <> sourceFile
      putStrLn $ "copy " <> sourceFile <> " to " <> targetFile
      copyFile sourceFile targetFile
      return (Right targetFile)
    else do
      return (Left NoSubmission)

-- | Create a directory
createEmptyStudent ::
  SubmissionId ->
  Assignment ->
  IO ()
createEmptyStudent sid suiteName = do
  let targetDir =
        assignmentCollectStudentDir sid suiteName errorStudent
  createDirectoryIfMissing True targetDir
