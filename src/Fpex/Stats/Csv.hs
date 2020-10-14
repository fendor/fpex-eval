module Fpex.Stats.Csv where

import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade.Paths
import Fpex.Grade.Types

data StatsCsvLine = StatsCsvLine
  { statsStudent :: Student,
    statsPoints :: Points,
    statsMaxPoints :: Points
  }

collectData :: [Student] -> SubmissionId -> SubmissionName -> IO [StatsCsvLine]
collectData students sid suiteName = forM students $ \student -> do
  let reportJson = reportSourceJsonFile sid suiteName student
  Just results@TestSuiteResults {..} <- Aeson.decodeFileStrict reportJson
  let statsStudent = student
  let statsPoints = testSuitePoints
  let statsMaxPoints = maxScore results
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
