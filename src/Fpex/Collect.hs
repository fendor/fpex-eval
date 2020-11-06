module Fpex.Collect where

import Fpex.Course.Types
import Fpex.Grade.ErrorStudent (errorStudent)
import Fpex.Grade.Paths
import System.Directory
import qualified Data.Text as T

data FailureReason = NoSubmission | IOErrorReason IOError
  deriving (Show, Eq)

setTestSuite :: SubmissionId -> SubmissionName -> TestSuitePath -> IO ()
setTestSuite sid submissionName testSuite = do
  let targetDir = assignmentCollectDir sid submissionName
  testSuiteFp <- canonicalizePath $ getTestSuitePath testSuite
  createDirectoryIfMissing True targetDir

  -- copy assignment main
  putStrLn $ "copy " <> testSuiteFp <> " to " <> targetDir
  copyFile testSuiteFp (testSuiteMain sid submissionName)

prepareSubmissionFolder :: SubmissionId -> SubmissionName -> IO ()
prepareSubmissionFolder sid suiteName = do
  let targetDir = assignmentCollectDir sid suiteName
  createDirectoryIfMissing True targetDir

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
  , "> chmod g+r " <> T.pack (getStudentSubmission s)
  ]