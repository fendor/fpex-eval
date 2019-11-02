module Fpex.Main where

import qualified Data.Text.IO                  as T
import           Data.Aeson                     ( decodeFileStrict' )
import           Control.Monad                  ( forM_ )

import           Options.Applicative

import           Polysemy                       ( runM )
import           Fpex.Options
import           Fpex.Course.Types
import           Fpex.Eval.Main                as Eval
import           Fpex.Eval.Types               as Eval
import           Fpex.Eval.Effect              as Eval
import           Fpex.Eval.Pretty              as Eval
import           Fpex.Course.DirSetup          as Setup
import           Fpex.Collect                  as Collect
import           Fpex.Publish                  as Publish

defaultMain :: IO ()
defaultMain = do
    Options {..}            <- execParser options
    -- TODO: error effect
    Just course@Course {..} <- decodeFileStrict' optionCourseFile
    let students = maybe courseStudents pure optionStudent

    case optionCommand of
        Grade CommandGrade {..} -> do
            Just testSuite <- decodeFileStrict' testSuiteFile
            forM_ students $ \student@Student { matrNr } -> do
                T.putStrLn $ "grade student " <> matrNr
                testReport <-
                    runM
                    $ Eval.runGrade
                    $ Eval.runStudentData
                    $ Eval.evalStudent course testSuite student
                -- TODO: move this into grade-function?
                T.writeFile (Eval.reportCollectFile course testSuite student)
                    $ prettyTestReport testReport


        Setup -> Setup.dirSetup course
        Collect CollectCommand { collectTestSuiteFile } -> do
            Just testSuite <- decodeFileStrict' collectTestSuiteFile
            forM_ students $ Collect.collectAssignment course testSuite
        Publish PublishCommand { publishTestSuiteFile } -> do
            Just testSuite <- decodeFileStrict' publishTestSuiteFile
            forM_ students $ Publish.publishTestResult course testSuite
