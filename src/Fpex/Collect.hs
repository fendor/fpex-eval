module Fpex.Collect where

import Control.Exception.Safe
import Control.Monad.Extra (whenM)
import qualified Data.ByteString as BS
import Fpex.Course.Types
import Fpex.Grade.ErrorStudent (errorStudent)
import Fpex.Grade.Paths
import System.Directory
import System.FilePath
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (when)
import System.IO.Error (isPermissionError)

data FailureReason = NoSubmission | IOErrorReason IOError
  deriving (Show, Eq)

setTestSuite :: Course -> SubmissionId -> SubmissionName -> TestSuitePath -> IO ()
setTestSuite course sid submissionName testSuite = do
  let targetDir = assignmentCollectDir sid submissionName
  let testSuiteFp = getTestSuitePath testSuite
  createDirectoryIfMissing True targetDir

  -- copy assignment main
  putStrLn $ "copy " <> testSuiteFp <> " to " <> targetDir
  copyFile testSuiteFp (testSuiteMain course)

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

  hasErr <- try $ copySubmission sourceFile targetFile
  case hasErr of
    Right r -> pure r
    Left err -> do
      putStrLn $ show err
      when (isPermissionError err) $ do
        T.writeFile (takeDirectory sourceFile </> collectFileError) (permissionErrorStudentMessage studentSubmission)

      pure $ Left $ IOErrorReason err
  where
    collectFileError = getStudentSubmission studentSubmission ++ ".collect_error"

    copySubmission :: FilePath
                  -> FilePath -> IO (Either FailureReason FilePath)
    copySubmission sourceFile targetFile = do
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
          pure (Right targetFile)
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

-- ----------------------------------------------------------------------------
-- Error Messages
-- ----------------------------------------------------------------------------

permissionErrorStudentMessage :: StudentSubmission -> T.Text
permissionErrorStudentMessage s =
  T.unlines
  [ "Your submission \"" <> T.pack (getStudentSubmission s) <> "\" could not be collected because of insufficient permissions."
  , "Please make sure that the permissions are set appropriately. The group \"fptutor\" needs at least read access to your submission."
  , ""
  , "You can set read file permissions via:"
  , "> chmod +0044 " <> T.pack (getStudentSubmission s)
  ]