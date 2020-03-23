module Fpex.Course.Types where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           System.FilePath

data Course = Course
    { courseName :: Text
    -- | The root-directory of the course. Contains home-dirs of the users and the `admin` directory
    , courseRootDir :: FilePath
    , courseUserPrefix :: Text
    , courseParticipants :: [Student]
    , courseGhciOptions :: [String]
    , courseGhciDependencies :: [Text]
    , courseGhciEnvironment :: FilePath
    }
    deriving (Ord, Eq, Show, Generic)

courseJsonModifier :: String -> String
courseJsonModifier = drop (length ("course_" :: String)) . camelTo2 '_'

instance FromJSON Course where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = courseJsonModifier
        }

instance ToJSON Course where
    toJSON =
        genericToJSON defaultOptions { fieldLabelModifier = courseJsonModifier }

newtype Student = Student
    { matrNr :: Text
    }
    deriving (Ord, Eq, Show, Generic)
    deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

courseAdminDir :: Course -> FilePath
courseAdminDir Course { courseRootDir } = courseRootDir </> "admin"

ghciEnvironmentLocation :: Course -> FilePath
ghciEnvironmentLocation c@Course { courseGhciEnvironment } =
    courseAdminDir c </> courseGhciEnvironment

studentDir :: Course -> Student -> FilePath
studentDir Course { courseRootDir, courseUserPrefix } Student { matrNr } =
    courseRootDir </> T.unpack (courseUserPrefix <> matrNr)
