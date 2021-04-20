module GradeResultParser where

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as LBS
import Fpex.Grade.Tasty (decodeFileTastyGradingReport)
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

goldenParserTests :: TestTree
goldenParserTests = testGroup "Parse reporter results" $
  flip map resultParserTests $ \input ->
    goldenVsString ("Parse: " ++ input) (goldenParserTestsRoot </> input <.> "golden") $ do
      r <- decodeFileTastyGradingReport (goldenParserTestsInputRoot </> input)
      case r of
        Left err -> pure $ LBS.pack err
        Right res -> pure $ Pretty.encodePretty res

resultParserTests :: [FilePath]
resultParserTests = ["basic.json", "not-submitted.json", "overflow.json", "only-errors.json"]

goldenParserTestsRoot :: FilePath
goldenParserTestsRoot = "testdata/goldenResultParser"

goldenParserTestsInputRoot :: FilePath
goldenParserTestsInputRoot = "testdata/goldenResultParser/input"