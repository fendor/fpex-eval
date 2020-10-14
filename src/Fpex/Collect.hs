module Fpex.Collect where

import Control.Monad.Extra (whenM)
import qualified Data.ByteString as BS
import Fpex.Course.Types
import Fpex.Grade.Types
import System.Directory
import System.FilePath
import System.IO
import Fpex.Grade.ErrorStudent (errorStudent)
import Control.Exception.Safe

data FailureReason = NoSubmission | IOErrorReason IOError
  deriving (Show, Eq)

setTestSuite :: SubmissionId -> SubmissionName -> TestSuitePath -> IO ()
setTestSuite sid submissionName testSuite = do
  let targetDir = assignmentCollectDir sid submissionName
  let testSuiteFp = getTestSuitePath testSuite
  createDirectoryIfMissing True targetDir

  -- copy assignment main
  let testSuiteTarget = targetDir </> "Main.hs"
  putStrLn $ "copy " <> testSuiteFp <> " to " <> targetDir
  copyFile testSuiteFp testSuiteTarget

prepareSubmissionFolder :: SubmissionId -> SubmissionName -> IO ()
prepareSubmissionFolder sid suiteName = do
  let targetDir = assignmentCollectDir sid suiteName
  createDirectoryIfMissing True targetDir

collectSubmission :: Course -> SubmissionId -> SubmissionName -> StudentSubmission -> Student -> IO (Either FailureReason FilePath)
collectSubmission course sid submissionName studentSubmission student = do
  let sourceFile = studentSourceFile course studentSubmission student
  let targetDir = assignmentCollectStudentDir' sid submissionName student
  let targetFile = assignmentCollectStudentFile sid submissionName studentSubmission student
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
      hasErr <- tryDeep $ copyFile sourceFile targetFile
      case hasErr of
        Left err -> do
          putStrLn $ show err
          pure (Left $ IOErrorReason err)
        Right () -> pure (Right targetFile)
    else do
      return (Left NoSubmission)

-- | Create a directory
createEmptyStudent ::
  SubmissionId ->
  SubmissionName ->
  IO ()
createEmptyStudent sid suiteName = do
  let targetDir =
        assignmentCollectStudentDir' sid suiteName errorStudent
  createDirectoryIfMissing True targetDir
