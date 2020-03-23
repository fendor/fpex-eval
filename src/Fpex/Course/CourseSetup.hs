module Fpex.Course.CourseSetup where

import           Data.Text                      ( Text )
import           Data.List                      ( inits )
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as BL
import           Control.Monad                  ( when )
import qualified Data.Aeson.Encode.Pretty      as Aeson
import           Data.Maybe                     ( mapMaybe
                                                )

import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                )
import           System.FilePath                ( splitPath
                                                , joinPath
                                                , takeFileName
                                                , dropTrailingPathSeparator
                                                )
import           Text.Regex.TDFA

import           Polysemy
import           Polysemy.Error

import           Fpex.Course.Types
import           Fpex.Options

-- |To be called from the courses admin folder
courseSetup
    :: (Member (Error Text) r, Member (Embed IO) r) => SetupCommand -> Sem r ()
courseSetup SetupCommand {..} = do

    courseDir <- findCourseRoot SetupCommand {..}
    let courseName = takeFileName $ dropTrailingPathSeparator courseDir
    students <- findParticipants SetupCommand {..} courseDir
    let course = Course { courseName             = T.pack $ courseName
                        , courseRootDir          = courseDir
                        , courseParticipants     = students
                        , courseGhciOptions      = ["+RTS", "-M500M", "-K10M", "-RTS"]
                        , courseGhciDependencies = ["base", "array"]
                        , courseGhciEnvironment  = ".ghc.environment.fpex"
                        }
    embed $ BL.writeFile "course.json" $ Aeson.encodePretty course


findParticipants :: Member (Embed IO) r => SetupCommand -> FilePath -> Sem r [Student]
findParticipants SetupCommand {..} courseDir = do  
    studentDirs <- embed $ listDirectory courseDir
    return $ mapMaybe (parseStudentDir setupUserRegex) studentDirs

findCourseRoot :: Members [Error Text, Embed IO] r => SetupCommand -> Sem r FilePath
findCourseRoot SetupCommand {..} = 
    case setupCourseRootDir of 
        Nothing -> do 
            -- check if in correct directory
            currentDir <- embed getCurrentDirectory
            let a = ancestors currentDir
            when (length a < 2)
                $ throw ("setup should be called from admin directory" :: Text)
            let courseDir  = a !! 1
            return courseDir

        Just dir -> return dir

ancestors :: FilePath -> [FilePath]
ancestors = map joinPath . reverse . inits . splitPath

parseStudentDir :: String -> FilePath -> Maybe Student
parseStudentDir userRegex dir 
    | dir =~ userRegex = Just $ Student $ T.pack dir
    | otherwise = Nothing
