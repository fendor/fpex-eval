module Fpex.Course.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Course = Course
  { courseName :: Text,
    -- | The root-directory of the course. Contains home-dirs of the users and the `admin` directory
    courseRootDir :: FilePath,
    courseParticipants :: [Student],
    courseGhciOptions :: [String],
    courseGhciDependencies :: [Text],
    courseGhciEnvironment :: FilePath
  }
  deriving (Ord, Eq, Show, Generic)

courseJsonModifier :: String -> String
courseJsonModifier = drop (length ("course_" :: String)) . camelTo2 '_'

instance FromJSON Course where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = courseJsonModifier
        }

instance ToJSON Course where
  toJSON =
    genericToJSON defaultOptions {fieldLabelModifier = courseJsonModifier}

newtype Student = Student
  { studentId :: Text
  }
  deriving (Ord, Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

ghciEnvironmentLocation :: Course -> FilePath
ghciEnvironmentLocation Course {courseGhciEnvironment} = courseGhciEnvironment
