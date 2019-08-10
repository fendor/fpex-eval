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
    , courseHomeDir :: FilePath
    , courseStudents :: [Student]
    , courseGroups :: [Group]
    }
    deriving (Ord, Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Student = Student
    { matrNr :: Text
    }
    deriving (Ord, Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
    deriving newtype (FromJSONKey, ToJSONKey)

data Group = Group
    { groupName :: Text
    , groupStudents :: [Student]
    }
    deriving (Ord, Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

studentHomedir :: Course -> Student -> FilePath
studentHomedir Course { courseName, courseHomeDir } Student { matrNr } =
    courseHomeDir </> T.unpack (courseName <> matrNr)
