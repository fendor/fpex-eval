module Fpex.Course.CourseSetup where

import           Data.Text                      ( Text )
import           Data.List                      ( inits )
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as BL
import           Control.Monad                  ( when )
import qualified Data.Aeson.Encode.Pretty      as Aeson
import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )

import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                )
import           System.FilePath                ( splitPath
                                                , joinPath
                                                , takeFileName
                                                )
import           Text.Regex.TDFA

import           Polysemy
import           Polysemy.Error

import           Fpex.Course.Types
import           Fpex.Options

-- |To be called from the courses admin folder
courseSetup :: (Member (Error Text) r, Member (Embed IO) r) => SetupCommand -> Sem r ()
courseSetup SetupCommand {..} = do

    -- check if in correct directory
    currentDir <- embed getCurrentDirectory
    when (takeFileName currentDir /= "admin")
        $ throw ("setup should be called from admin directly" :: Text)

    a <- embed ancestors
    when (length a < 2)
        $ throw ("setup should be called from admin directory" :: Text)
    let courseDir  = a !! 1
    let courseName = takeFileName courseDir

    studentDirs <- embed $ listDirectory courseDir
    let students = mapMaybe (parseStudentDir $ T.pack prefix) studentDirs

    let course = Course { courseName       = T.pack courseName
                        , courseRootDir    = courseDir
                        , courseGroups     = []
                        , courseUserPrefix = T.pack prefix
                        , courseStudents   = students
                        }
    embed $ BL.writeFile "course.json" $ Aeson.encodePretty course



ancestors :: IO [FilePath]
ancestors = map joinPath . reverse . inits . splitPath <$> getCurrentDirectory

parseStudentDir :: Text -> FilePath -> Maybe Student
parseStudentDir userPrefix dir =
    let (_, _, _, mNr) :: (Text, Text, Text, [Text]) =
                T.pack dir =~ (userPrefix <> "([0-9]{8})")
    in  Student <$> listToMaybe mNr
