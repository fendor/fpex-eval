module Fpex.Course.Types where

import           Data.Text                                ( Text )
import qualified Data.Text as T
import           GHC.Generics                             ( Generic )
import           Data.Aeson                               ( FromJSON
                                                          , ToJSON
                                                          , FromJSONKey
                                                          , ToJSONKey
                                                          )
import           System.FilePath

data Course = Course
    { courseName :: Text
    -- | The root-directory of the course. Contains home-dirs of the users and the `admin` directory
    , courseRootDir :: FilePath
    , courseGroups :: [Group]
    , courseUserPrefix :: Text
    , courseStudents :: [Student]
    }
    deriving (Ord, Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Student = Student
    { matrNr :: Text
    }
    deriving (Ord, Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Group = Group
    { groupName :: Text
    , groupStudents :: [Student]
    }
    deriving (Ord, Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

courseAdminDir :: Course -> FilePath
courseAdminDir Course { courseRootDir } = courseRootDir </> "admin"

studentDir :: Course -> Student -> FilePath
studentDir Course {courseRootDir, courseUserPrefix} Student {matrNr} =
    courseRootDir </> T.unpack (courseUserPrefix <> matrNr)
