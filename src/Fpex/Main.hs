module Fpex.Main where

import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Aeson                    as Aeson
import           Data.List                      ( inits )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Extra            ( findM )

import           Options.Applicative

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Fpex.Options
import           Fpex.Course.Types
import qualified Fpex.Grade                    as Grade
import qualified Fpex.Course.CourseSetup       as Setup
import qualified Fpex.Collect                  as Collect
import qualified Fpex.Publish                  as Publish
import qualified Fpex.Eval.Types               as Eval
import qualified Fpex.Stats.Histogram          as Histogram
import           System.IO                      ( stderr )
import           System.Exit                    ( exitFailure )
import           System.Directory               ( getCurrentDirectory
                                                , doesFileExist
                                                )
import           System.FilePath                ( splitPath
                                                , joinPath
                                                , (</>)
                                                )


defaultMain :: IO ()
defaultMain = (runM . runError $ defaultMain') >>= \case
    Right () -> return ()
    Left  e  -> do
        T.hPutStrLn stderr e
        exitFailure

defaultMain' :: (Member (Error Text) r, Member (Embed IO) r) => Sem r ()
defaultMain' = do
    Options {..} <- embed $ execParser options

    case optionCommand of
        Grade CommandGrade {..} -> do
            let TestSuiteOptions {..} = gradeTestSuiteOptions
            course@Course {..} <- getCourseConfig optionCourseFile
            
            runReader course $ do
                let testSuiteName = optionTestSuiteSpecification
                let students      = maybe courseStudents pure optionStudent

                (compileFailTestSuite, noSubmissionTestSuite)
                    <- Grade.runGradeError (Grade.createEmptyStudent optionSubmissionId course testSuiteName)
                        >>= \case
                        Right (a, b) -> return (a, b)
                        Left err -> throw (T.pack $ show err)

                forM_ students $ \student -> do
                    let targetFile = Eval.reportSourceJsonFile optionSubmissionId course testSuiteName student
                    Grade.runGradeError (Grade.runSubmission optionSubmissionId
                                                course
                                                testSuiteName
                                                student)
                        >>= \case
                            Right () -> return ()
                            Left err -> embed $ do
                                T.putStrLn (T.pack $ show err)
                                case err of
                                    Grade.RunnerFailedToCompile ->
                                        Aeson.encodeFile targetFile compileFailTestSuite
                                    Grade.NoSubmission ->
                                        Aeson.encodeFile targetFile noSubmissionTestSuite
                    return ()

        Setup   setupCommand -> Setup.courseSetup setupCommand
        Collect CollectCommand {..} -> do
            let TestSuiteOptions {..} = collectTestSuiteOptions
            course@Course {..} <- getCourseConfig optionCourseFile
            let testSuiteName = optionTestSuiteSpecification
            let students      = maybe courseStudents pure optionStudent

            embed $ Collect.prepareSubmissionFolder optionSubmissionId
                                                    course
                                                    testSuiteName

            forM_ students $ \student -> do
                embed $ Collect.collectSubmission optionSubmissionId
                                                  course
                                                  testSuiteName
                                                  student
                return ()
        Publish PublishCommand {..} -> do
            let TestSuiteOptions {..} = publishTestSuiteOptions
            course@Course {..} <- getCourseConfig optionCourseFile
            let testSuiteName = optionTestSuiteSpecification
            let students = maybe courseStudents pure optionStudent

            forM_ students $ \student -> do
                embed $ Publish.publishTestResult optionSubmissionId
                                                  course
                                                  testSuiteName
                                                  student
                return ()
        Stats StatCommand {..} -> do
            let TestSuiteOptions {..} = statTestSuiteOptions
            course@Course {..} <- getCourseConfig optionCourseFile
            let testSuiteName = optionTestSuiteSpecification
            histoData <- embed (Histogram.compute optionSubmissionId course testSuiteName)
            embed (putStrLn $ Histogram.asciiArt 20 histoData)

-- | get the default course file
getDefaultCourseFile :: IO (Maybe FilePath)
getDefaultCourseFile = do
    ancestors <-
        map joinPath . reverse . inits . splitPath <$> getCurrentDirectory
    let possibleCourseJsonFiles = map (</> "course.json") ancestors
    findM doesFileExist possibleCourseJsonFiles

getCourseFile
    :: (Member (Error Text) r, Member (Embed IO) r)
    => Maybe FilePath
    -> Sem r (Maybe FilePath)
getCourseFile (Just c) = return $ Just c
getCourseFile Nothing  = embed getDefaultCourseFile

getCourseConfig
    :: (Member (Error Text) r, Member (Embed IO) r)
    => Maybe FilePath
    -> Sem r Course
getCourseConfig courseOption = do
    courseFile <- getCourseFile courseOption >>= \case
        Just c  -> return c
        Nothing -> throw ("course.json not found" :: Text)
    embed (Aeson.decodeFileStrict courseFile) >>= \case
        Just c  -> return c
        Nothing -> throw ("course.json invalid format" :: Text)

