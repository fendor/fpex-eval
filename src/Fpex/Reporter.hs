module Fpex.Reporter where


import Polysemy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Clock


withReport :: Member (Embed IO) r => T.Text -> Sem r a -> Sem r a
withReport label action = do
  embed $ T.putStrLn $ "!!! " <> label
  before <- embed $ getTime Monotonic
  !a <- action
  now <- embed $ getTime Monotonic
  let diff :: Double = (/ 100) . fromInteger . round . (* 100) $ diffSeconds before now
  embed $ T.putStrLn $ "### Execution time: " <> T.pack (show diff) <> "s"
  pure a
  where
    diffSeconds :: TimeSpec -> TimeSpec -> Double
    diffSeconds end start
      = (* 1e-9)
      $ fromIntegral
      $ toNanoSecs end - toNanoSecs  start


