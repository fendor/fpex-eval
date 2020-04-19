module Fpex.Publish where

import           System.Directory
import           Control.Monad.Extra                      ( whenM )
import           Control.Exception.Extra                  ( errorIO )
import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as T
import qualified Data.Text as T

import           Fpex.Course.Types
import           Fpex.Eval.Types
import qualified Fpex.Publish.Pretty as Publish


-- | collect assignment of single student
publishTestResult :: SubmissionId -> Course -> T.Text -> Student -> IO ()
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
