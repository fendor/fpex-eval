module Fpex.Reporter where


import Polysemy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Clock
import Fpex.Grade.Types


withReport :: Member (Embed IO) r => T.Text -> Sem r a -> Sem r a
withReport label action = do
  embed $ T.putStrLn $ "!!! " <> label
  before <- embed $ getTime Monotonic
  !a <- action
  now <- embed $ getTime Monotonic
  let diff :: Double = (/ 100) . fromInteger . round . (* 100) $ diffSeconds now before
  embed $ T.putStrLn $ "### Execution time: " <> T.pack (show diff) <> "s"
  pure a
  where
    diffSeconds :: TimeSpec -> TimeSpec -> Double
    diffSeconds end start
      = (* 1e-9)
      $ fromIntegral
      $ toNanoSecs end - toNanoSecs  start

prettyTestReport :: Member (Embed IO) r => TestSuiteResults -> Sem r ()
prettyTestReport testResult
  | isCompileFailReport testResult =
    embed $ T.putStrLn "Runner Error"
  | isNotSubmittedReport testResult =
    embed $ T.putStrLn "No Submission"
  | otherwise = do
    embed $
      T.putStrLn $
        T.unlines $
          map
            ("\t" <>)
            [ "Test Report:",
              "",
              T.concat
                [ "Points: ",
                  T.pack (show $ testSuitePoints testResult),
                  "/",
                  T.pack (show $ maxScore testResult)
                ],
              "",
              "Correct:       " <> (T.pack . show $ correctTests testResult),
              "Incorrect:     " <> (T.pack . show $ failedTests testResult),
              "Not submitted: " <> (T.pack . show $ notSubmittedTests testResult),
              "Timeout:       " <> (T.pack . show $ timeoutTests testResult)
            ]