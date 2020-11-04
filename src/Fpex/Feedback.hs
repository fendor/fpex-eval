{-# LANGUAGE TemplateHaskell #-}

module Fpex.Feedback where

import qualified Colog.Polysemy as Log
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fpex.Course.Types
import Fpex.Grade.Paths
import Fpex.Grade.Storage
import qualified Fpex.Publish.Plain as Publish
import Polysemy
import Polysemy.Internal (send)
import Polysemy.Reader
import qualified Text.RE.TDFA.Text as Regex

data Feedback m a where
  GenerateTestFeedback :: Feedback m ()

generateTestFeedback :: Member Feedback r => Sem r ()
generateTestFeedback = send GenerateTestFeedback

runFeedbackService ::
  Members
    [ Log.Log T.Text,
      Storage,
      Embed IO,
      Reader SubmissionInfo,
      Reader Course,
      Reader StudentSubmission
    ]
    r =>
  Sem (Feedback : r) a ->
  Sem r a
runFeedbackService = interpret $ \case
  GenerateTestFeedback -> do
    _fp <- generateTestResultFeedback
    pure ()

generateTestResultFeedback ::
  Members
    [ Log.Log T.Text,
      Storage,
      Embed IO,
      Reader StudentSubmission,
      Reader SubmissionInfo
    ]
    r =>
  Sem r FilePath
generateTestResultFeedback = do
  sinfo@SubmissionInfo {..} <- ask
  studentSubmission <- ask
  let targetFile = reportFeedbackFile subId subName studentSubmission subStudent
  Log.log $ "write " <> T.pack targetFile
  testSuiteResults <- readTestSuiteResult sinfo
  let prettyTextReport = Publish.prettyTestReport testSuiteResults
  embed (T.writeFile targetFile prettyTextReport)
  testSuiteContents <- embed $ T.readFile (testSuiteMain subId subName)
  testSuiteLoc <- reportTestSuiteFile
  embed (T.writeFile testSuiteLoc $ hackyPostProcessTestSuite testSuiteContents)
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
