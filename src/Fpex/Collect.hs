module Fpex.Collect where

import Control.Monad.Extra (whenM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fpex.Course.Types
import Fpex.Eval.Types
import System.Directory
import System.FilePath
import System.IO

data FailureReason = NoSubmission
  deriving (Show, Eq, Read)

setTestSuite :: SubmissionId -> T.Text -> FilePath -> IO ()
setTestSuite sid suiteName testSuite = do
  let targetDir = assignmentCollectDir sid suiteName
  createDirectoryIfMissing True targetDir

  -- copy assignment main
  let testSuiteTarget = targetDir </> "Main.hs"
  putStrLn $ "copy " <> testSuite <> " to " <> targetDir
  copyFile testSuite testSuiteTarget

prepareSubmissionFolder :: SubmissionId -> T.Text -> IO ()
prepareSubmissionFolder sid suiteName = do
  let targetDir = assignmentCollectDir sid suiteName
  createDirectoryIfMissing True targetDir

  -- copy test-spec file
  let testSpecTargetDir = targetDir </> "TestSpec.hs"
  testSpecFile <- makeAbsolute "TestSpec.hs"
  putStrLn $ "copy " <> testSpecFile <> " to " <> testSpecTargetDir
  copyFile testSpecFile testSpecTargetDir

  return ()

collectSubmission :: SubmissionId -> Course -> T.Text -> Student -> IO (Either FailureReason FilePath)
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
      whenM (BS.isInfixOf "unsafePerformIO" <$> BS.readFile sourceFile)
        $ hPutStrLn stderr
        $ "Warning: `unsafePerformIO` in submission "
          <> sourceFile
      putStrLn $ "copy " <> sourceFile <> " to " <> targetFile
      copyFile sourceFile targetFile
      return (Right targetFile)
    else do
      return (Left NoSubmission)

-- | Create a directory
createEmptyStudent ::
  SubmissionId ->
  T.Text ->
  IO (Either FailureReason FilePath)
createEmptyStudent sid suiteName = do
  let errorStudent = Student "errorStudent"
  let targetDir =
        assignmentCollectStudentDir sid suiteName errorStudent
  let targetFile =
        assignmentCollectStudentFile sid suiteName errorStudent
  createDirectoryIfMissing True targetDir
  T.writeFile targetFile ("module " <> suiteName <> " where\n")
  return $ Right targetFile
