module Fpex.Grade.Storage where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade.Paths
import Fpex.Grade.Result
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import System.Directory (doesFileExist)

data Storage m a where
  WriteTestSuiteResult :: SubmissionInfo -> TestSuiteResults -> Storage m ()
  ReadTestSuiteResult :: SubmissionInfo -> Storage m TestSuiteResults
  DoesTestSuiteResultExist :: SubmissionInfo -> Storage m Bool
  SubmissionLocation :: SubmissionInfo -> StudentSubmission -> Storage m (Maybe FilePath)

writeTestSuiteResult :: Member Storage r => SubmissionInfo -> TestSuiteResults -> Sem r ()
writeTestSuiteResult info s = send $ WriteTestSuiteResult info s

readTestSuiteResult :: Member Storage r => SubmissionInfo -> Sem r TestSuiteResults
readTestSuiteResult info = send $ ReadTestSuiteResult info

doesTestSuiteResultExist :: Member Storage r => SubmissionInfo -> Sem r Bool
doesTestSuiteResultExist info = send $ DoesTestSuiteResultExist info

submissionLocation :: Member Storage r => SubmissionInfo -> StudentSubmission -> Sem r (Maybe FilePath)
submissionLocation info submission = send $ SubmissionLocation info submission

runStorageFileSystem ::
  Members [Embed IO, Error T.Text] r =>
  Sem (Storage : r) a ->
  Sem r a
runStorageFileSystem = interpret $ \case
  WriteTestSuiteResult SubmissionInfo {..} results ->
    embed $ writeTestSuiteResultIO subId subName subStudent results
  ReadTestSuiteResult SubmissionInfo {..} ->
    readTestSuiteResultIO subId subName subStudent
  DoesTestSuiteResultExist sinfo ->
    doesSubmissionInfoExist sinfo
  SubmissionLocation SubmissionInfo {..} studentSubmission -> do
    let targetFile = assignmentCollectStudentFile subId subName subStudent studentSubmission
    embed (doesFileExist targetFile) >>= \case
      False -> pure Nothing
      True -> pure $ Just targetFile


readTestSuiteResultIO ::
  Members [Embed IO, Error T.Text] r =>
  SubmissionId ->
  SubmissionName ->
  Student ->
  Sem r TestSuiteResults
readTestSuiteResultIO sid suiteName student = do
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
  SubmissionName ->
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
  let studentFile = reportSourceJsonFile subId subName subStudent
  embed (doesFileExist studentFile)
