module Fpex.Publish.Stats where

import Fpex.Eval.Types
import Fpex.Course.Types
import Fpex.Stats.Csv
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad (forM)

data PointsCalc
  = Mean
  | Max

allPoints :: Course -> [SubmissionId] -> [T.Text] -> IO (Map.Map Student [(SubmissionId, T.Text, Points)])
allPoints course sids suites = do
  res <- forM sids $ \sid ->
    forM suites $ \suite -> do
      csvData <- collectData sid course suite
      pure $ map (\csv -> (statsStudent csv, [(sid,suite,statsPoints csv)])) csvData
  let r = concat $ concat res
  pure $ Map.fromListWith (++) r