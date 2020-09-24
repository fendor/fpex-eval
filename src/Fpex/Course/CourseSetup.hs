module Fpex.Course.CourseSetup where

import Control.Monad (when)
import Control.Monad.Extra (unlessM)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (inits)
import Data.Maybe
  ( mapMaybe,
  )
import Data.Text (Text)
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Options
import Polysemy
import Polysemy.Error
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    getCurrentDirectory,
    listDirectory,
  )
import System.FilePath
  ( dropTrailingPathSeparator,
    joinPath,
    splitPath,
    takeFileName,
  )
import Text.Regex.TDFA

-- | To be called from the courses admin folder
courseSetup ::
  (Member (Error Text) r, Member (Embed IO) r) => SetupCommand -> Sem r ()
courseSetup SetupCommand {..} = do
  courseDir <- findCourseRoot SetupCommand {..}
  unlessM (embed $ doesDirectoryExist courseDir) $
    throw $
      "The course root directory \""
        <> T.pack courseDir
        <> "\" does not exist."

  let courseName = takeFileName $ dropTrailingPathSeparator courseDir
  students <- findParticipants SetupCommand {..} courseDir
  when (null students) $
    throw $
      "No student can be found for this course\n"
        <> "\tDirectory: "
        <> T.pack courseDir
        <> "\n\tregex: "
        <> T.pack setupUserRegex

  let course =
        Course
          { courseName = T.pack $ courseName,
            courseRootDir = courseDir,
            courseParticipants = students,
            courseGhciOptions = ["+RTS", "-M500M", "-K10M", "-RTS"],
            courseGhciDependencies = ["base", "array"],
            courseGhciEnvironment = ".ghc.environment.fpex"
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
      when (length a < 2) $
        throw ("setup should be called from admin directory" :: Text)
      let courseDir = a !! 1
      return courseDir
    Just dir -> embed $ canonicalizePath dir

ancestors :: FilePath -> [FilePath]
ancestors = map joinPath . reverse . inits . splitPath

parseStudentDir :: String -> FilePath -> Maybe Student
parseStudentDir userRegex dir
  | dir =~ userRegex = Just $ Student $ T.pack dir
  | otherwise = Nothing
