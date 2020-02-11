module Fpex.Publish where

import           System.Directory
import           Control.Monad.Extra                      ( whenM )
import           Control.Exception.Extra                  ( errorIO )
import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as T

import           Fpex.Course.Types
import           Fpex.Eval.Types
import qualified Fpex.Publish.Types as Publish
import qualified Fpex.Publish.Pretty as Publish


-- | collect assignment of single student
publishTestResult :: SubmissionId -> Course -> String -> Student -> IO ()
publishTestResult sid course testSuite student = do

    let sourceFile = reportSourceJsonFile sid course testSuite student
    let targetFile = reportPublishFile sid course testSuite student

    whenM (doesFileExist sourceFile) $ do
        putStrLn $ "publish " <> sourceFile
        testSuiteResults <- Aeson.eitherDecodeFileStrict sourceFile 
          >>= \case 
          Right (r :: Publish.TestSuiteResults) -> return r 
          Left msg -> errorIO $ "Could not decode \"" ++ sourceFile ++ "\": " ++ msg

        let prettyTextReport = Publish.prettyTestReport testSuiteResults
        T.writeFile targetFile prettyTextReport 
