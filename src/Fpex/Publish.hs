{-# LANGUAGE TemplateHaskell #-}

module Fpex.Publish where

import qualified Colog.Polysemy as Log
import Control.Monad
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fpex.Course.Types
import Fpex.Grade.Paths
import Fpex.Grade.Storage
import qualified Fpex.Publish.Plain as Publish
import Polysemy
import Polysemy.Error
import Polysemy.Internal (send)
import Polysemy.Reader
import System.Directory
import System.FilePath
import qualified Text.RE.TDFA.Text as Regex

data FeedbackAction
  = WriteFeedback
  | PublishFeedback
  deriving (Show, Eq, Ord, Bounded)

data Publisher m a where
  WriteTestFeedback :: SubmissionInfo -> FeedbackAction -> Publisher m ()

writeTestFeedback :: Member Publisher r => SubmissionInfo -> FeedbackAction -> Sem r ()
writeTestFeedback sinfo s = send (WriteTestFeedback sinfo s)

runPublisherService :: Members [Log.Log T.Text, Storage, Embed IO, Error T.Text, Reader Course, Reader StudentSubmission] r => Sem (Publisher : r) a -> Sem r a
runPublisherService = interpret $ \case
  WriteTestFeedback sinfo s -> do
    _fp <- writeTestResultFeedback sinfo
    when (PublishFeedback <= s) $ do
      publishTestResult sinfo

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
  embed
    ( copyFile
        (reportTestSuiteFile subId subName studentSubmission subStudent)
        (studentTestSuiteFile subId course studentSubmission subStudent)
    )
  copyHandwrittenFeedback course subId subName subStudent

writeTestResultFeedback ::
  Members [Log.Log T.Text, Storage, Embed IO, Reader StudentSubmission] r =>
  SubmissionInfo ->
  Sem r FilePath
writeTestResultFeedback sinfo@SubmissionInfo {..} = do
  studentSubmission <- ask
  let targetFile = reportFeedbackFile subId subName studentSubmission subStudent
  Log.log $ "write " <> T.pack targetFile
  testSuiteResults <- readTestSuiteResult sinfo
  let prettyTextReport = Publish.prettyTestReport testSuiteResults
  embed (T.writeFile targetFile prettyTextReport)
  testSuiteContents <- embed $ T.readFile (testSuiteMain subId subName)
  embed (T.writeFile (reportTestSuiteFile subId subName studentSubmission subStudent) $ hackyPostProcessTestSuite testSuiteContents)
  pure targetFile

hackyPostProcessTestSuite :: T.Text -> T.Text
hackyPostProcessTestSuite t =
  t
    & testGroupPropsMatcher
    & T.lines
    & filter isNotImportLine
    & map mainFunction
    & T.unlines
  where
    isNotImportLine = not . ("Test.Tasty.Grade" `T.isInfixOf`)
    mainFunction l = T.replace "composeReporters consoleTestReporter jsonRunner" "consoleTestReporter" l

testGroupPropsMatcher :: T.Text -> T.Text
testGroupPropsMatcher t = t Regex.*=~/ [Regex.ed|([A-Z][A-Za-z]*\.)?testGroupPoints [0-9]+ [0-9]+ [0-9]+ (\$ )?///|]

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
