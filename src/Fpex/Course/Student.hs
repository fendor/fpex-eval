module Fpex.Course.Student
  ( StudentDirectory,
    runFileSystemStudentDirectory,
    collectStudentSubmission,
    publishSubmissionFeedback,
    publishTestSuite,
    publishTestSuiteResult,
    publishFile,
  )
where

import qualified Colog.Polysemy as Log
import Control.Exception
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fpex.Course.Types
import Fpex.Grade.Paths
import Fpex.Grade.Storage
import Polysemy
import Polysemy.Internal
import Polysemy.Reader
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error (isPermissionError)
import qualified System.PosixCompat.Files as Posix

data StudentDirectory m a where
  CollectStudentSubmission :: SubmissionInfo -> StudentSubmission -> StudentDirectory m (Maybe FailureReason)
  PublishTestSuiteResult :: SubmissionInfo -> StudentSubmission -> StudentDirectory m ()
  PublishTestSuite :: SubmissionInfo -> StudentSubmission -> FilePath -> StudentDirectory m ()
  PublishSubmissionFeedback :: SubmissionInfo -> StudentSubmission -> StudentDirectory m ()
  PublishFile :: T.Text -> FilePath -> StudentDirectory m ()

data FailureReason = NoSubmission | IOErrorReason IOError
  deriving (Show, Eq)

collectStudentSubmission :: Member StudentDirectory r => SubmissionInfo -> StudentSubmission -> Sem r (Maybe FailureReason)
collectStudentSubmission sinfo studentSubmission = send (CollectStudentSubmission sinfo studentSubmission)

publishTestSuiteResult :: Member StudentDirectory r => SubmissionInfo -> StudentSubmission -> Sem r ()
publishTestSuiteResult sinfo studentSubmission = send (PublishTestSuiteResult sinfo studentSubmission)

publishTestSuite :: Member StudentDirectory r => SubmissionInfo -> StudentSubmission -> FilePath -> Sem r ()
publishTestSuite sinfo studentSubmission fp = send (PublishTestSuite sinfo studentSubmission fp)

publishSubmissionFeedback :: Member StudentDirectory r => SubmissionInfo -> StudentSubmission -> Sem r ()
publishSubmissionFeedback sinfo studentSubmission = send (PublishSubmissionFeedback sinfo studentSubmission)

publishFile :: Member StudentDirectory r => T.Text -> FilePath -> Sem r ()
publishFile contents fp = send (PublishFile contents fp)

runFileSystemStudentDirectory ::
  Members
    [ Embed IO,
      Log.Log T.Text,
      Storage,
      Reader Course,
      Reader Student
    ]
    r =>
  Sem (StudentDirectory : r) a ->
  Sem r a
runFileSystemStudentDirectory = interpret $ \case
  CollectStudentSubmission SubmissionInfo {..} studentSubmission -> do
    course <- ask
    collectSubmission course subId subName studentSubmission subStudent
  PublishTestSuiteResult submissionInfo studentSubmission -> runReader studentSubmission $ do
    publishTestResult submissionInfo
  PublishTestSuite sinfo studentSubmission fp -> runReader sinfo $
    runReader studentSubmission $ do
      testSuiteLocation <- studentTestSuiteFile
      embed
        ( copyFile
            fp
            testSuiteLocation
        )

      embed $
        Posix.setFileMode
          testSuiteLocation
          Posix.stdFileMode
  PublishSubmissionFeedback SubmissionInfo {..} _ -> do
    course <- ask
    copyHandwrittenFeedback course subId subName subStudent
  PublishFile contents fp -> do
    course <- ask
    student <- ask
    let targetDir = studentSubDir course student
    embed $ T.putStrLn $ "Publish file: " <> T.pack fp <> " to " <> T.pack targetDir
    embed $ T.writeFile (targetDir </> fp) contents

collectSubmission :: Members [Storage, Embed IO] r => Course -> SubmissionId -> SubmissionName -> StudentSubmission -> Student -> Sem r (Maybe FailureReason)
collectSubmission course sid submissionName studentSubmission student = do
  let sourceFile = studentSourceFile course studentSubmission student
  let targetDir = assignmentCollectStudentDir' sid submissionName student
  let targetFile = assignmentCollectStudentFile sid submissionName student studentSubmission
  -- create submission dir even if there is no submission
  embed $ createDirectoryIfMissing True targetDir

  hasErr <- embed $ try $ copySubmission sourceFile targetFile
  case hasErr of
    Right (Right _s) -> pure Nothing
    Right (Left err) -> pure $ Just err
    Left err -> embed $ do
      putStrLn $ show err
      when (isPermissionError err) $ do
        T.writeFile (takeDirectory sourceFile </> collectFileError) (permissionErrorStudentMessage studentSubmission)

      pure $ Just $ IOErrorReason err
  where
    collectFileError = getStudentSubmission studentSubmission ++ ".collect_error"

    copySubmission ::
      FilePath ->
      FilePath ->
      IO (Either FailureReason FilePath)
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

-- | Publish assignment of single student
publishTestResult ::
  Members [Log.Log T.Text, Storage, Embed IO, Reader Course, Reader StudentSubmission] r =>
  SubmissionInfo ->
  Sem r ()
publishTestResult SubmissionInfo {..} = do
  course <- ask
  studentSubmission <- ask
  let fp = reportFeedbackFile subId subName studentSubmission subStudent
  let targetFile = reportPublishFile subId course studentSubmission subStudent
  Log.log $ "publish " <> T.pack targetFile
  embed $ copyFile fp targetFile
  embed $
    Posix.setFileMode
      targetFile
      Posix.stdFileMode

copyHandwrittenFeedback :: Members [Log.Log T.Text, Embed IO] r => Course -> SubmissionId -> SubmissionName -> Student -> Sem r ()
copyHandwrittenFeedback course sid suiteName student = do
  let sourceDir = assignmentCollectStudentDir' sid suiteName student
  let targetDir = studentDir course student
  let feedbackFile = sourceDir </> "Feedback.md"
  let feedbackTarget = targetDir </> (T.unpack (getSubmissionName suiteName <> "_Feedback")) <.> "md"
  exists <- embed $ doesFileExist feedbackFile
  when exists $ do
    Log.log $ "publish feedback " <> T.pack feedbackFile
    embed $ copyFile feedbackFile feedbackTarget

-- ----------------------------------------------------------------------------
-- Error Messages
-- ----------------------------------------------------------------------------

permissionErrorStudentMessage :: StudentSubmission -> T.Text
permissionErrorStudentMessage s =
  T.unlines
    [ "Your submission \"" <> T.pack (getStudentSubmission s) <> "\" could not be collected because of insufficient permissions.",
      "Please make sure that the permissions are set appropriately. The group \"fptutor\" needs at least read access to your submission.",
      "",
      "You can set read file permissions via:",
      "> chmod g+r " <> T.pack (getStudentSubmission s)
    ]

-- ----------------------------------------------------------------------------
-- Filepath utilities for accessing student directories
-- ----------------------------------------------------------------------------

-- | Root directory of the real student
studentDir :: Course -> Student -> FilePath
studentDir Course {..} Student {..} =
  courseRootDir </> T.unpack studentId

-- | Absolute path to the real student's sub-directory to which feedback can be delivered.
studentSubDir :: Course -> Student -> FilePath
studentSubDir Course {..} Student {..} =
  normalise $
    studentDir Course {..} Student {..}
      </> fromMaybe "." courseStudentSubDir

-- | Filename of the submission file
studentSourceFile :: Course -> StudentSubmission -> Student -> FilePath
studentSourceFile course studentSubmission student =
  studentDir course student </> getStudentSubmission studentSubmission

-- | Location of the grading result.
reportPublishFile ::
  SubmissionId ->
  Course ->
  StudentSubmission ->
  Student ->
  FilePath
reportPublishFile sid course studentSubmission student =
  studentSubDir course student
    </> reportName sid studentSubmission

studentTestSuiteFile :: Members [Reader Course, Reader SubmissionInfo, Reader StudentSubmission] r => Sem r FilePath
studentTestSuiteFile =
  studentTestSuiteFile' <$> ask <*> asks subId <*> ask <*> asks subStudent

-- | Location of the test-suite delivered to the student.
studentTestSuiteFile' ::
  Course ->
  SubmissionId ->
  StudentSubmission ->
  Student ->
  FilePath
studentTestSuiteFile' course sid studentSubmission student =
  studentSubDir course student
    </> testSuiteName sid studentSubmission
