module Fpex.Publish where

import Control.Exception.Extra (errorIO)
import Control.Monad.Extra (whenM)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fpex.Course.Types
import Fpex.Grade.Types
import qualified Fpex.Publish.Plain as Publish
import System.Directory
import System.FilePath

-- | Publish assignment of single student
publishTestResult :: SubmissionId -> Course -> Assignment -> Student -> IO ()
publishTestResult sid course suiteName student = do
  let sourceFile = reportSourceJsonFile sid suiteName student
  let targetFile = reportPublishFile sid course suiteName student
  whenM (doesFileExist sourceFile) $ do
    putStrLn $ "publish " <> sourceFile
    testSuiteResults <- Aeson.eitherDecodeFileStrict sourceFile
      >>= \case
        Right r -> return r
        Left msg -> errorIO $ "Could not decode \"" ++ sourceFile ++ "\": " ++ msg
    let prettyTextReport = Publish.prettyTestReport testSuiteResults
    T.writeFile targetFile prettyTextReport
  let sourceDir = assignmentCollectStudentDir sid suiteName student
  let targetDir = studentDir course student
  let feedbackFile = sourceDir </> "Feedback.md"
  let feedbackTarget = targetDir </> (T.unpack (getAssignment suiteName) <> "_Feedback") <.> "md"
  whenM (doesFileExist feedbackFile) $ do
    copyFile feedbackFile feedbackTarget
