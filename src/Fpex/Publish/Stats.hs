module Fpex.Publish.Stats where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Eval.Types

data PointsCalc
  = Mean
  | Max

allPoints :: [Student] -> [T.Text] -> [SubmissionId] -> IO (Map.Map Student (Map.Map (T.Text, SubmissionId) (Points, Points)))
allPoints students suites sids = do
  r <- forM students $ \student -> studentPointReport suites sids student >>= pure . (student,)
  pure $ Map.fromList r

studentPointReport :: [T.Text] -> [SubmissionId] -> Student -> IO (Map.Map (T.Text, SubmissionId) (Points, Points))
studentPointReport suites sids student = do
  let tuples = (,) <$> suites <*> sids
  r <- forM tuples $ \(suite, sid) -> do
    Just r <- readTestSuiteResult' sid suite student
    pure $ ((suite, sid), (testSuitePoints r, maxScore r))
  pure $ Map.fromList r

renderPoints :: [T.Text] -> [SubmissionId] -> PointsCalc -> Student -> Map.Map (T.Text, SubmissionId) (Points, Points) -> T.Text
renderPoints suitesOrder sids calcSubmissionPoints student studentData =
  T.unlines $
    [ "# Final Points for " <> studentId student,
      "",
      "",
      renderTableRow headerRow,
      separator
    ]
      ++ map mdRow sids
      ++ [ renderTableRow (zip cellLengths summaryRow),
           "",
           "",
           "Final Points: " <> T.pack (show $ fst pointSummary)
             <> " / "
             <> T.pack (show $ snd pointSummary)
         ]
  where
    pointSummary :: (Points, Points)
    pointSummary = (sum $ map fst finalPoints, sum $ map snd finalPoints)
    finalPoints :: [(Points, Points)]
    finalPoints =
      map
        ( \suite ->
            let ps =
                  map
                    (\sid -> (studentData Map.! (suite, sid)))
                    sids
             in ( calculatePoints
                    calcSubmissionPoints
                    (map fst ps),
                  head $ map snd ps
                )
        )
        suitesOrder
    mdRow :: SubmissionId -> T.Text
    mdRow sid = renderTableRow $ zip cellLengths (dataRow sid)
    dataRow :: SubmissionId -> [T.Text]
    dataRow sid =
      [T.pack (show $ getSubmissionId sid)]
        <> map
          ( \suite ->
              showPoints
                (studentData Map.! (suite, sid))
          )
          suitesOrder
    summaryRow :: [T.Text]
    summaryRow =
      ["Final"]
        <> map showPoints finalPoints
    showPoints :: (Points, Points) -> T.Text
    showPoints (s, _) = T.pack (show s) -- <> " / " <> T.pack (show s)
    header :: [T.Text]
    header = "Id \\ TestSuite" : suitesOrder
    headerRow :: [(Int, T.Text)]
    headerRow = zip cellLengths header
    cellLengths :: [Int]
    cellLengths = map T.length header
    separator :: T.Text
    separator = renderTableRow $ map (\l -> (l, T.replicate l "-")) cellLengths
    cellSeparator :: T.Text
    cellSeparator = " | "
    renderTableRow :: [(Int, T.Text)] -> T.Text
    renderTableRow xs =
      let row = T.intercalate cellSeparator $ map cellContents xs
       in "| " <> row <> " |"
      where
        cellContents (cellWidth, t) = spaces <> t
          where
            wordLength = T.length t
            spaces = T.replicate (cellWidth - wordLength) " "

calculatePoints :: PointsCalc -> [Int] -> Int
calculatePoints Mean [] = 0
calculatePoints Mean xs =
  ceiling
    ( (fromIntegral (sum xs) :: Double)
        / fromIntegral (length xs)
    )
calculatePoints Max [] = 0
calculatePoints Max xs = maximum xs
