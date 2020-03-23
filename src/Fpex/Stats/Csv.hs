module Fpex.Stats.Csv where


import           Fpex.Eval.Types
import qualified Data.Text                     as T
import qualified Data.Aeson                    as Aeson
import           Data.Text                      ( Text )
import           Fpex.Course.Types
import           Control.Monad                  ( forM )

data StatsCsvLine = StatsCsvLine
    { statsStudent :: Student
    , statsPoints :: Points
    , statsMaxPoints :: Points
    }

collectData :: SubmissionId -> Course -> FilePath -> IO [StatsCsvLine]
collectData sid Course {..} testSuite = forM courseParticipants $ \student -> do
    let reportJson = reportSourceJsonFile sid Course { .. } testSuite student
    Just results@TestSuiteResults {..} <- Aeson.decodeFileStrict reportJson
    let statsStudent   = student
    let statsPoints    = testSuitePoints
    let statsMaxPoints = maxScore results
    return $ StatsCsvLine { .. }

csvLineString :: StatsCsvLine -> Text
csvLineString StatsCsvLine {..} = T.intercalate
    ";"
    [ matrNr statsStudent
    , T.pack $ show statsPoints
    , T.pack $ show statsMaxPoints
    ]

statsCsv :: [StatsCsvLine] -> Text
statsCsv stats =
    "student;points;maxPoints" <> T.intercalate "\n" (map csvLineString stats)

