module Fpex.Grade.Storage where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade.Types
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import System.Directory (doesFileExist)

data TestSuiteStorage m a where
  WriteTestSuiteResult :: SubmissionInfo -> TestSuiteResults -> TestSuiteStorage m ()
  ReadTestSuiteResult :: SubmissionInfo -> TestSuiteStorage m TestSuiteResults
  DoesTestSuiteResultExist :: SubmissionInfo -> TestSuiteStorage m Bool

writeTestSuiteResult :: Member TestSuiteStorage r => SubmissionInfo -> TestSuiteResults -> Sem r ()
writeTestSuiteResult info s = send $ WriteTestSuiteResult info s

readTestSuiteResult :: Member TestSuiteStorage r => SubmissionInfo -> Sem r TestSuiteResults
readTestSuiteResult info = send $ ReadTestSuiteResult info

doesTestSuiteResultExist :: Member TestSuiteStorage r => SubmissionInfo -> Sem r Bool
doesTestSuiteResultExist info = send $ DoesTestSuiteResultExist info

runTestSuiteStorageFileSystem ::
  Members [Embed IO, Error T.Text] r =>
  Sem (TestSuiteStorage : r) a ->
  Sem (r) a
runTestSuiteStorageFileSystem = interpret $ \case
  WriteTestSuiteResult SubmissionInfo {..} results ->
    embed $ writeTestSuiteResultIO subId subTestSuite subStudent results
  ReadTestSuiteResult SubmissionInfo {..} ->
    readTestSuiteResultIO subId subTestSuite subStudent
  DoesTestSuiteResultExist sinfo ->
    doesSubmissionInfoExist sinfo

readTestSuiteResultIO ::
  Members [Embed IO, Error T.Text] r =>
  SubmissionId ->
  Assignment ->
  Student ->
  Sem r TestSuiteResults
readTestSuiteResultIO sid suiteName student =
  readTestSuiteResult' sid suiteName student

readTestSuiteResult' ::
  Members [Embed IO, Error T.Text] r =>
  SubmissionId ->
  Assignment ->
  Student ->
  Sem r TestSuiteResults
readTestSuiteResult' sid suiteName student = do
  let sourceFile = reportSourceJsonFile sid suiteName student
  embed
    ( Aeson.eitherDecodeFileStrict sourceFile
    )
    >>= \case
      Left decodeError ->
        throw $
          "Could not decode \""
            <> T.pack sourceFile
            <> "\": "
            <> T.pack decodeError
      Right e -> pure e

writeTestSuiteResultIO ::
  SubmissionId ->
  Assignment ->
  Student ->
  TestSuiteResults ->
  IO ()
writeTestSuiteResultIO sid suiteName student testSuiteResults =
  LBS.writeFile
    (reportSourceJsonFile sid suiteName student)
    (Pretty.encodePretty testSuiteResults)


-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

doesSubmissionInfoExist :: Member (Embed IO) r => SubmissionInfo -> Sem r Bool
doesSubmissionInfoExist SubmissionInfo {..} = do
  let studentFile = reportSourceJsonFile subId subTestSuite subStudent
  embed (doesFileExist studentFile)
