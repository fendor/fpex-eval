module Fpex.Stats.Csv where

import Control.Monad (forM)
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

collectData :: Member Storage r => [Student] -> SubmissionId -> SubmissionName -> Sem r [StatsCsvLine]
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

statsCsv :: [StatsCsvLine] -> Text
statsCsv stats =
  "student;points;maxPoints" <> T.intercalate "\n" (map csvLineString stats)
