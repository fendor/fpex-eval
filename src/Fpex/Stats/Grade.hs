module Fpex.Stats.Grade
  ( statsGrade,
  )
where

import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Fpex.Stats.Csv

-- data StatsCsvLine = StatsCsvLine
--     { statsStudent :: Student
--     , statsPoints :: Points
--     , statsMaxPoints :: Points
--     }

data StatGrade = NotSubmitted | StatGrade Int
  deriving (Eq, Show, Ord)

statsGrade :: [StatsCsvLine] -> Text
statsGrade stats =
  T.intercalate "\n" $
    "Grades:" :
    ( stats
        & map (\s -> (statSubmissionGrade s, 1 :: Integer))
        & Map.fromListWith (+)
        & printStats
    )
  where
    printStats :: Map StatGrade Integer -> [Text]
    printStats m =
      [ "1: " <> T.pack (show $ Map.findWithDefault 0 (StatGrade 1) m),
        "2: " <> T.pack (show $ Map.findWithDefault 0 (StatGrade 2) m),
        "3: " <> T.pack (show $ Map.findWithDefault 0 (StatGrade 3) m),
        "4: " <> T.pack (show $ Map.findWithDefault 0 (StatGrade 4) m),
        "5: " <> T.pack (show $ Map.findWithDefault 0 (StatGrade 5) m)
          <> " ("
          <> T.pack (show $ Map.findWithDefault 0 NotSubmitted m)
          <> ")"
      ]

statSubmissionGrade :: StatsCsvLine -> StatGrade
statSubmissionGrade StatsCsvLine {..}
  | Just points <- statsPoints = StatGrade $ toGrade points statsMaxPoints
  | otherwise = NotSubmitted
  where
    toGrade points maxPoints
      | ratio < 0.5 = 5
      | ratio < 0.625 = 4
      | ratio < 0.75 = 3
      | ratio < 0.875 = 2
      | otherwise = 1
      where
        ratio = fromIntegral points / fromIntegral maxPoints :: Double
