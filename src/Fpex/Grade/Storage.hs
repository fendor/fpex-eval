module Fpex.Grade.Storage where


import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy as LBS
import Fpex.Course.Types
import Fpex.Grade.Types
import Polysemy
import Polysemy.Internal

data TestSuiteStorage m a where
  WriteTestSuiteResult :: SubmissionInfo -> TestSuiteResults -> TestSuiteStorage m ()
  ReadTestSuiteResult :: SubmissionInfo -> TestSuiteStorage m (Maybe TestSuiteResults)

writeTestSuiteResult :: Member TestSuiteStorage r => SubmissionInfo -> TestSuiteResults -> Sem r ()
writeTestSuiteResult info s = send $ WriteTestSuiteResult info s

readTestSuiteResult :: Member TestSuiteStorage r => SubmissionInfo -> Sem r (Maybe TestSuiteResults)
readTestSuiteResult info = send $ ReadTestSuiteResult info

runTestSuiteStorageFileSystem :: Member (Embed IO) r => Sem (TestSuiteStorage : r) a -> Sem (r) a
runTestSuiteStorageFileSystem = interpret $ \case
  WriteTestSuiteResult SubmissionInfo {..} results ->
    embed $ writeTestSuiteResultIO subId subTestSuite subStudent results
  ReadTestSuiteResult SubmissionInfo {..} ->
    embed $ readTestSuiteResultIO subId subTestSuite subStudent

readTestSuiteResultIO ::
  SubmissionId ->
  Assignment ->
  Student ->
  IO (Maybe TestSuiteResults)
readTestSuiteResultIO sid suiteName student =
  Aeson.decodeFileStrict'
    (reportSourceJsonFile sid suiteName student)

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
