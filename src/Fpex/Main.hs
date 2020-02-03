module Fpex.Main where

import qualified Data.Text.IO                  as T
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Aeson                    as Aeson
import           Data.List                      ( inits )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Extra            ( findM )

import           Options.Applicative

import           Polysemy
import           Polysemy.Reader                ( runReader )
import           Fpex.Options
import           Fpex.Course.Types
import qualified Fpex.Eval.Main                as Eval
import qualified Fpex.Eval.Types               as Eval
import           Fpex.Eval.Types                ( TestSuite(..) )
import qualified Fpex.Eval.Effect              as Eval
import qualified Fpex.Eval.Pretty              as Eval
import qualified Fpex.EdslGrade                as EdslGrade
import qualified Fpex.Log.Effect               as Log
import qualified Fpex.Course.CourseSetup       as Setup
import qualified Fpex.Collect                  as Collect
import qualified Fpex.Publish                  as Publish
import qualified Fpex.Parse.Input              as Parser
import           System.IO                      ( stderr )
import           System.Exit                    ( exitFailure )
import           System.Directory               ( getCurrentDirectory
                                                , doesFileExist
                                                )
import           System.FilePath                ( splitPath
                                                , joinPath
                                                , (</>)
                                                )

import           Polysemy.Error

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
        EdslGrade TestSuiteOptions {..} -> do
            course@Course {..} <- getCourseConfig optionCourseFile
            let EdslSpec testSuiteName = optionTestSuiteSpecification
            let students               = maybe courseStudents pure optionStudent

            embed $ EdslGrade.prepareSubmissionFolder optionSubmissionId
                                                      course
                                                      testSuiteName

            forM_ students $ \student -> do
                embed $ EdslGrade.collectSubmission optionSubmissionId
                                                    course
                                                    testSuiteName
                                                    student
                return ()
            forM_ students $ \student -> do
                embed $ EdslGrade.linkSubmission optionSubmissionId
                                                 course
                                                 testSuiteName
                                                 student
                return ()
            forM_ students $ \student -> do
                embed $ EdslGrade.runSubmission optionSubmissionId
                                                course
                                                testSuiteName
                                                student
                return ()

        Grade CommandGrade {..} -> do
            course@Course {..} <- getCourseConfig optionCourseFile
            let students = maybe courseStudents pure optionStudent

            testSuite <- getTestSuiteFile
                $ optionTestSuiteSpecification gradeTestSuiteOptions
            forM_ students $ \student -> do
                testReport <-
                    Log.runLog
                    $ runReader (optionSubmissionId gradeTestSuiteOptions)
                    $ runReader testTimeout
                    $ Eval.runGrade gradeRunner
                    $ Eval.runStudentData
                    $ Eval.evalStudent course testSuite student

                -- TODO: move this into grade-function?
                embed
                    $ T.writeFile
                          (Eval.reportCollectFile
                              (optionSubmissionId gradeTestSuiteOptions)
                              course
                              testSuite
                              student
                          )
                    $ Eval.prettyTestReport testReport

                embed $ Aeson.encodeFile
                    (Eval.reportJsonFile
                        (optionSubmissionId gradeTestSuiteOptions)
                        course
                        testSuite
                        student
                    )
                    testReport

        Setup -> Setup.courseSetup
        Collect CollectCommand { collectTestSuiteOptions } -> do
            course@Course {..} <- getCourseConfig optionCourseFile
            let students = maybe courseStudents pure optionStudent

            testSuite <- getTestSuiteFile
                $ optionTestSuiteSpecification collectTestSuiteOptions
            embed $ forM_ students $ Collect.collectAssignment
                (optionSubmissionId collectTestSuiteOptions)
                course
                testSuite
        Publish PublishCommand { publishTestSuiteOptions } -> do
            course@Course {..} <- getCourseConfig optionCourseFile
            let students = maybe courseStudents pure optionStudent

            testSuite <- getTestSuiteFile
                $ optionTestSuiteSpecification publishTestSuiteOptions
            embed $ forM_ students $ Publish.publishTestResult
                (optionSubmissionId publishTestSuiteOptions)
                course
                testSuite


getTestSuiteFile
    :: (Member (Error Text) r, Member (Embed IO) r)
    => TestSuiteSpecification
    -> Sem r TestSuite
getTestSuiteFile (Legacy fp ass) =
    embed (Parser.parseTestSpecification' fp) >>= \case
        Left  err -> throw $ T.pack $ show err
        Right s   -> return $ TestSuite ass s
getTestSuiteFile (Json fp) = embed (Aeson.decodeFileStrict' fp) >>= \case
    Just s  -> return s
    Nothing -> throw ("unable to read test suite file" :: Text)



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

