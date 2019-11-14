module Fpex.Main where

import qualified Data.Text.IO                  as T
import qualified Data.Aeson                    as Aeson
import           Control.Monad                  ( forM_ )

import           Options.Applicative

import           Polysemy
import           Polysemy.Reader                ( runReader )
import           Fpex.Options
import           Fpex.Course.Types
import           Fpex.Eval.Main                as Eval
import           Fpex.Eval.Types               as Eval
import           Fpex.Eval.Effect              as Eval
import           Fpex.Eval.Pretty              as Eval
import           Fpex.Log.Effect               as Log
import           Fpex.Course.DirSetup          as Setup
import           Fpex.Collect                  as Collect
import           Fpex.Publish                  as Publish
import qualified Fpex.Parse.Input              as Parser
import           System.IO                      ( stderr
                                                , hPrint
                                                , hPutStrLn
                                                )
import           System.Exit                    ( exitFailure )

defaultMain :: IO ()
defaultMain = do
    Options {..}            <- execParser options
    -- TODO: error effect
    Just course@Course {..} <- Aeson.decodeFileStrict' optionCourseFile
    let students = maybe courseStudents pure optionStudent

    case optionCommand of
        Grade CommandGrade {..} -> do
            testSuiteM <- getTestSuiteFile testSuiteSpec
            case testSuiteM of
                Nothing -> do
                    hPutStrLn stderr "Could not decode test-suite specification"
                    exitFailure
                Just testSuite -> forM_ students $ \student -> do
                    testReport <-
                        runM
                        $ Log.runLog
                        $ runReader testTimeout
                        $ Eval.runGrade gradeRunner
                        $ Eval.runStudentData
                        $ Eval.evalStudent course testSuite student

                    -- TODO: move this into grade-function?
                    T.writeFile
                            (Eval.reportCollectFile course testSuite student)
                        $ prettyTestReport testReport

                    Aeson.encodeFile
                        (Eval.reportCollectFile course testSuite student)
                        testReport


        Setup -> Setup.dirSetup course
        Collect CollectCommand { collectTestSuiteFile } -> do
            Just testSuite <- getTestSuiteFile collectTestSuiteFile
            forM_ students $ Collect.collectAssignment course testSuite
        Publish PublishCommand { publishTestSuiteFile } -> do
            Just testSuite <- getTestSuiteFile publishTestSuiteFile
            forM_ students $ Publish.publishTestResult course testSuite


getTestSuiteFile :: TestSuiteSpecification -> IO (Maybe TestSuite)
getTestSuiteFile (Legacy fp ass) = Parser.parseTestSpecification' fp >>= \case
    Left err -> do
        hPrint stderr err
        return Nothing
    Right s -> return $ Just $ TestSuite ass s
getTestSuiteFile (Json fp) = Aeson.decodeFileStrict' fp
