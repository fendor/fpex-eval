module Fpex.Main where

import Data.Either
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Aeson                    as Aeson
import           Control.Monad                  ( forM_, forM )
import           Control.Monad.Extra            (unlessM,  findM )

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
import qualified Fpex.Stats.Csv                as Stats
import qualified Fpex.Stats.Grade              as Stats
import           System.IO                      ( stderr )
import           System.Exit                    ( exitFailure )
import           System.Directory               ( getCurrentDirectory
                                                , setCurrentDirectory
                                                , doesFileExist
                                                )
import           System.FilePath                ( (</>)
                                                , takeDirectory
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
        Setup   setupCommand        -> Setup.courseSetup setupCommand
        Grade GradeCommand {..} -> do
            let TestSuiteOptions {..} = gradeTestSuiteOptions
            (Course {..}, courseDir) <- getCourseConfig optionCourseFile
            embed $ setCurrentDirectory courseDir
            runReader Course {..} $ do
                let testSuiteSpecification = optionTestSuiteSpecification
                let students      = maybe courseParticipants pure optionStudent

                checkTestSuiteExists testSuiteSpecification

                (compileFailTestSuite, noSubmissionTestSuite) <-
                    Grade.runGradeError
                            (Grade.createEmptyStudent optionSubmissionId
                                                      testSuiteSpecification
                            )
                        >>= \case
                                Right (a, b) -> return (a, b)
                                Left  err    -> throw (T.pack $ show err)

                forM_ students $ \student -> do
                    let targetFile = Eval.reportSourceJsonFile
                            optionSubmissionId
                            testSuiteSpecification
                            student
                    Grade.runGradeError
                            (Grade.runSubmission optionSubmissionId
                                                 testSuiteSpecification
                                                 student
                            )
                        >>= \case
                                Right ()  -> return ()
                                Left  err -> embed $ do
                                    T.putStrLn ("\t" <> T.pack (show err))
                                    case err of
                                        Grade.RunnerFailedToCompile ->
                                            Aeson.encodeFile
                                                targetFile
                                                compileFailTestSuite
                                        Grade.NoSubmission -> Aeson.encodeFile
                                            targetFile
                                            noSubmissionTestSuite
                    return ()

        Collect CollectCommand {..} -> do
            let TestSuiteOptions {..} = collectTestSuiteOptions
            (Course {..}, courseDir) <- getCourseConfig optionCourseFile
            embed $ setCurrentDirectory courseDir
            let testSuiteSpecification = optionTestSuiteSpecification
            let students = maybe courseParticipants pure optionStudent

            checkTestSuiteExists testSuiteSpecification

            embed $ Collect.prepareSubmissionFolder optionSubmissionId
                                                    testSuiteSpecification

            collectResults <- forM students $ \student -> do
                embed $ Collect.collectSubmission optionSubmissionId
                                                  Course {..}
                                                  testSuiteSpecification
                                                  student

            let (_errs, collected) = partitionEithers collectResults

            embed $ putStrLn $ "Collected " <> show (length collected) <> "/" <> show (length students) <> " submissions."


        Publish PublishCommand {..} -> do
            let TestSuiteOptions {..} = publishTestSuiteOptions
            (Course {..}, courseDir) <- getCourseConfig optionCourseFile
            embed $ setCurrentDirectory courseDir
            let testSuiteSpecification = optionTestSuiteSpecification
            let students      = maybe courseParticipants pure optionStudent

            checkTestSuiteExists testSuiteSpecification

            forM_ students $ \student -> do
                embed $ Publish.publishTestResult optionSubmissionId
                                                  Course {..}
                                                  testSuiteSpecification
                                                  student
                return ()
        Stats StatCommand {..} -> do
            let TestSuiteOptions {..} = statTestSuiteOptions
            (Course {..}, courseDir) <- getCourseConfig optionCourseFile
            embed $ setCurrentDirectory courseDir
            let testSuiteSpecification = optionTestSuiteSpecification

            checkTestSuiteExists testSuiteSpecification

            stats <- embed
                (Stats.collectData optionSubmissionId Course {..} testSuiteSpecification)
            case statOutputKind of
                StatsOutputCsv -> embed (T.putStrLn $ Stats.statsCsv stats)
                StatsOutputGrades ->
                    embed (T.putStrLn $ Stats.statsGrade stats)

-- | get the default course file
getDefaultCourseFile :: IO (Maybe FilePath)
getDefaultCourseFile = do
    cwd <- getCurrentDirectory
    let possibleCourseJsonFiles = map (</> "course.json") (Setup.ancestors cwd)
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
    -> Sem r (Course, FilePath)
getCourseConfig courseOption = do
    courseFile <- getCourseFile courseOption >>= \case
        Just c  -> return c
        Nothing -> throw "course.json not found"
    embed (Aeson.decodeFileStrict courseFile) >>= \case
        Just c  -> return (c, takeDirectory courseFile)
        Nothing -> throw "course.json invalid format"

checkTestSuiteExists :: Members [Error Text, Embed IO] r => FilePath -> Sem r ()
checkTestSuiteExists testSuite = 
    unlessM (embed $ doesFileExist testSuite) 
        $ throw $ "Test-Suite specification \"" 
            <> T.pack testSuite 
            <> "\" does not exist"