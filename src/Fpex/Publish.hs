module Fpex.Publish where

import qualified Colog.Polysemy as Log
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fpex.Course.Types
import Fpex.Grade.Storage
import Fpex.Grade.Types
import qualified Fpex.Publish.Plain as Publish
import Polysemy
import Polysemy.Error
import Polysemy.Internal (send)
import Polysemy.Reader
import System.Directory
import System.FilePath

data FeedbackState =
  WriteFeedback
  | PublishFeedback
  deriving (Show, Eq, Ord, Bounded)

data Publisher m a where
  WriteTestFeedback :: SubmissionInfo -> FeedbackState -> Publisher m ()

writeTestFeedback :: Member Publisher r => SubmissionInfo -> FeedbackState -> Sem r ()
writeTestFeedback sinfo s = send (WriteTestFeedback sinfo s)

runPublisherService :: Members [Log.Log T.Text, Storage, Embed IO, Error T.Text, Reader Course] r => Sem (Publisher : r) a -> Sem r a
runPublisherService = interpret $ \case
  WriteTestFeedback sinfo s -> do
    _fp <- writeTestResultFeedback sinfo
    when (PublishFeedback <= s) $ do
      publishTestResult sinfo

-- | Publish assignment of single student
publishTestResult ::
  Members [Log.Log T.Text, Storage, Embed IO, Error T.Text, Reader Course] r =>
  SubmissionInfo ->
  Sem r ()
publishTestResult SubmissionInfo {..} = do
  course <- ask
  let fp = reportFeedbackFile subId subName subStudent
  let targetFile = reportPublishFile subId course subName subStudent
  embed $ copyFile fp targetFile
  copyHandwrittenFeedback course subId subName subStudent

writeTestResultFeedback ::
  Members [Log.Log T.Text, Storage, Embed IO, Error T.Text, Reader Course] r =>
  SubmissionInfo ->
  Sem r FilePath
writeTestResultFeedback sinfo@SubmissionInfo {..} = do
  let targetFile = reportFeedbackFile subId subName subStudent
  Log.log $ "publish " <> T.pack targetFile
  testSuiteResults <- readTestSuiteResult sinfo
  let prettyTextReport = Publish.prettyTestReport testSuiteResults
  embed (T.writeFile targetFile prettyTextReport)
  pure targetFile

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
