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

data Publisher m a where
  WriteTestFeedback :: SubmissionInfo -> Publisher m ()
  PublishTestFeedback :: SubmissionInfo -> Publisher m ()

writeTestFeedback :: Member Publisher r => SubmissionInfo -> Sem r ()
writeTestFeedback sinfo = send (WriteTestFeedback sinfo)

publishTestFeedback :: Member Publisher r => SubmissionInfo -> Sem r ()
publishTestFeedback sinfo = send (PublishTestFeedback sinfo)

runPublisherService :: Members [Log.Log T.Text, TestSuiteStorage, Embed IO, Error T.Text, Reader Course] r => Sem (Publisher : r) a -> Sem r a
runPublisherService = interpret $ \case
  WriteTestFeedback sinfo -> do
    _resultPath <- writeTestResultFeedback sinfo
    pure ()
  PublishTestFeedback sinfo ->
    publishTestResult sinfo

-- | Publish assignment of single student
publishTestResult ::
  Members [Log.Log T.Text, TestSuiteStorage, Embed IO, Error T.Text, Reader Course] r =>
  SubmissionInfo ->
  Sem r ()
publishTestResult SubmissionInfo {..} = do
  course <- ask
  let fp = reportFeedbackFile subId subTestSuite subStudent
  let targetFile = reportPublishFile subId course subTestSuite subStudent
  embed $ copyFile fp targetFile
  copyHandwrittenFeedback course subId subTestSuite subStudent

writeTestResultFeedback ::
  Members [Log.Log T.Text, TestSuiteStorage, Embed IO, Error T.Text, Reader Course] r =>
  SubmissionInfo ->
  Sem r FilePath
writeTestResultFeedback sinfo@SubmissionInfo {..} = do
  let targetFile = reportFeedbackFile subId subTestSuite subStudent
  Log.log $ "publish " <> T.pack targetFile
  testSuiteResults <- readTestSuiteResult sinfo
  let prettyTextReport = Publish.prettyTestReport testSuiteResults
  embed (T.writeFile targetFile prettyTextReport)
  pure targetFile

copyHandwrittenFeedback :: Members [Log.Log T.Text, Embed IO] r => Course -> SubmissionId -> Assignment -> Student -> Sem r ()
copyHandwrittenFeedback course sid suiteName student = do
  let sourceDir = assignmentCollectStudentDir sid suiteName student
  let targetDir = studentDir course student
  let feedbackFile = sourceDir </> "Feedback.md"
  let feedbackTarget = targetDir </> (T.unpack (getAssignment suiteName <> "_Feedback")) <.> "md"
  exists <- embed $ doesFileExist feedbackFile
  when exists $ do
    Log.log $ "publish feedback " <> T.pack feedbackFile
    embed $ copyFile feedbackFile feedbackTarget
