module Report where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text
import Fpex.Publish.Plain (prettyTestReport)
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

goldenReportTests :: TestTree
goldenReportTests = testGroup "Generate Report" $
  flip map reportTestNames $ \input ->
    goldenVsString ("Parse: " ++ input) (goldenReportRoot </> input -<.> "golden") $ do
      r <- Aeson.eitherDecodeFileStrict (goldenReportInputRoot </> input)
      case r of
        Left err -> pure $ LBS.pack err
        Right res -> pure $ LBS.fromStrict $ Text.encodeUtf8 $ prettyTestReport res

reportTestNames :: [FilePath]
reportTestNames = ["basic.json", "timeout.json", "overflow.json", "only-errors.json"]

goldenReportRoot :: FilePath
goldenReportRoot = "testdata/goldenReport"

goldenReportInputRoot :: FilePath
goldenReportInputRoot = "testdata/goldenReport/input"