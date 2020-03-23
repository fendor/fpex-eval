module Fpex.Stats.Histogram where


import           Fpex.Eval.Types
import qualified Data.Aeson                    as Aeson
import           Data.Array
import           Data.Function                  ( (&) )
import           Data.List                      ( transpose )
import           Fpex.Course.Types
import           Control.Monad                  ( forM )
import           Text.Printf                    ( printf )

compute :: SubmissionId -> Course -> FilePath -> IO [(Int, Double)]
compute sid Course {..} testSuite = do

    points <- forM courseParticipants $ \student -> do
        let reportJson =
                reportSourceJsonFile sid Course { .. } testSuite student
        Just TestSuiteResults {..} <- Aeson.decodeFileStrict reportJson
        return (testSuitePoints, maxScore TestSuiteResults { .. })

    case points of
        []                    -> return []
        (((_, maxscore) : _)) -> return $ histo maxscore (map fst points)

  where
    histo :: Int -> [Int] -> [(Int, Double)]
    histo maxscore ps =
        let total = fromIntegral $ length ps :: Double

            counts :: Array Int Int
            counts     = accumArray (+) 0 (0, maxscore) (map (, 1) ps)
            normalised = fmap ((/ total) . fromIntegral) counts
        in  assocs normalised

asciiArt
    :: Int -- ^ Height
    -> [(Int, Double)] -- ^ Data
    -> String
asciiArt height hData =
    hData
        & map
              (\(bin, size) -> printf "%3d_%s%s"
                                      bin
                                      (replicate (binSize size) '#')
                                      (replicate (emptyBin size) ' ')
              )
        & wrapBorder
        & transpose
        & reverse
        & zipWith (++) (yLabels ++ replicate 4 "    ")
        & unlines
  where
    binSize s = ceiling (s * fromIntegral height)
    emptyBin s = height - binSize s
    wrapBorder x = (border : x) ++ [border]
    border  = replicate (height + 4) '|'
    percents :: [Int]
    percents = map
        ((round :: Double -> Int ) . (*100) . (/ fromIntegral height) . fromIntegral)
        [height, (height - 1) .. 1]
    yLabels = map (printf "%3d%%") percents
