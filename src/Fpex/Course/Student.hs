module Fpex.Course.Student where

import qualified Colog.Polysemy as Log
import Control.Exception
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import qualified Data.ByteString as BS
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
  CollectStudentSubmission :: StudentDirectory m (Either FailureReason FilePath)
  PublishTestSuiteResult :: StudentDirectory m ()
  PublishTestSuite :: FilePath -> StudentDirectory m ()
  PublishSubmissionFeedback :: StudentDirectory m ()

data FailureReason = NoSubmission | IOErrorReason IOError
  deriving (Show, Eq)

collectStudentSubmission :: Member StudentDirectory r => Sem r (Either FailureReason FilePath)
collectStudentSubmission = send CollectStudentSubmission

publishTestSuiteResult :: Member StudentDirectory r => Sem r ()
publishTestSuiteResult = send PublishTestSuiteResult

publishTestSuite :: Member StudentDirectory r => FilePath -> Sem r ()
publishTestSuite fp = send (PublishTestSuite fp)

publishSubmissionFeedback :: Member StudentDirectory r => Sem r ()
publishSubmissionFeedback = send PublishSubmissionFeedback

runFileSystemStudentDirectory ::
  Members
    [ Embed IO,
      Log.Log T.Text,
      Storage,
      Reader Course,
      Reader StudentSubmission,
      Reader SubmissionInfo
    ]
    r =>
  Sem (StudentDirectory : r) a ->
  Sem r a
runFileSystemStudentDirectory = interpret $ \case
  CollectStudentSubmission -> do
    course <- ask
    SubmissionInfo {..} <- ask
    studentSubmission <- ask
    collectSubmission course subId subName studentSubmission subStudent
  PublishTestSuiteResult -> do
    submissionInfo <- ask
    publishTestResult submissionInfo
  PublishTestSuite fp -> do
    SubmissionInfo {..} <- ask
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
  PublishSubmissionFeedback -> do
    course <- ask
    SubmissionInfo {..} <- ask
    copyHandwrittenFeedback course subId subName subStudent

collectSubmission :: Members [Storage, Embed IO] r => Course -> SubmissionId -> SubmissionName -> StudentSubmission -> Student -> Sem r (Either FailureReason FilePath)
collectSubmission course sid submissionName studentSubmission student = do
  let sourceFile = studentSourceFile course studentSubmission student
  let targetDir = assignmentCollectStudentDir' sid submissionName student
  let targetFile = assignmentCollectStudentFile sid submissionName studentSubmission student
  -- create submission dir even if there is no submission
  embed $ createDirectoryIfMissing True targetDir

  hasErr <- embed $ try $ copySubmission sourceFile targetFile
  case hasErr of
    Right r -> pure r
    Left err -> embed $ do
      putStrLn $ show err
      when (isPermissionError err) $ do
        T.writeFile (takeDirectory sourceFile </> collectFileError) (permissionErrorStudentMessage studentSubmission)

      pure $ Left $ IOErrorReason err
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