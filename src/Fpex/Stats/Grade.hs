module Fpex.Stats.Grade
    ( statsGrade
    )
where


import qualified Data.Text                     as T
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import           Data.Array

import           Fpex.Stats.Csv

-- data StatsCsvLine = StatsCsvLine
--     { statsStudent :: Student
--     , statsPoints :: Points
--     , statsMaxPoints :: Points
--     }

newtype StatGrade = StatGrade Int
    deriving newtype (Eq, Show, Num, Ix)

instance Ord StatGrade where
    (StatGrade a) `compare` (StatGrade b) = b `compare` a



statsGrade :: [StatsCsvLine] -> Text
statsGrade stats =
    T.intercalate "\n"
        $ "Grades:"
        : ( stats
          & map (\s -> (statSubmissionGrade s, 1 :: Integer))
          & accumArray (+) 0 (1, 5)
          & assocs
          & map (\(g, n) -> T.pack (show g) <> ": " <> T.pack (show n))
          )


statSubmissionGrade :: StatsCsvLine -> StatGrade
statSubmissionGrade StatsCsvLine {..} | ratio < 0.5   = 5
                                      | ratio < 0.625 = 4
                                      | ratio < 0.75  = 3
                                      | ratio < 0.875 = 2
                                      | otherwise     = 1
  where
    ratio = fromIntegral statsPoints / fromIntegral statsMaxPoints :: Double
