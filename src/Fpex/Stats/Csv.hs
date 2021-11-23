module Fpex.Stats.Csv where

import Control.Monad (forM)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade.Result
import Fpex.Grade.Storage
import Polysemy

data StatsCsvLine = StatsCsvLine
  { statsStudent :: Student,
    statsPoints :: Maybe Points,
    statsMaxPoints :: Points
  }

collectData :: (Member Storage r, Traversable f) => f Student -> SubmissionId -> SubmissionName -> Sem r (f StatsCsvLine)
collectData students sid suiteName = forM students $ \student -> do
  result <- readTestSuiteResult (SubmissionInfo student sid suiteName)
  let statsStudent = student
  let statsPoints = if isNotSubmittedReport result then Nothing else Just (testSuitePoints result)
  let statsMaxPoints = maxScore result
  return $ StatsCsvLine {..}

csvLineString :: StatsCsvLine -> Text
csvLineString StatsCsvLine {..} =
  T.intercalate
    ";"
    [ studentId statsStudent,
      T.pack $ show statsPoints,
      T.pack $ show statsMaxPoints
    ]

statsCsv :: Traversable f => f StatsCsvLine -> Text
statsCsv stats =
  "student;points;maxPoints" <> T.intercalate "\n" (toList $ fmap csvLineString stats)
