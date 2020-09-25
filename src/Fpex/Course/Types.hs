module Fpex.Course.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
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

newtype SubmissionId = SubmissionId {getSubmissionId :: Int}
  deriving (Show, Generic)
  deriving newtype (Eq, Num, Ord)

newtype TestSuitePath = TestSuiteTestSuitePath
  {getTestSuitePath :: FilePath}
  deriving (Show, Eq, Ord)

newtype Assignment = Assignment
  {getAssignment :: T.Text}
  deriving (Show, Eq, Ord)

data SubmissionInfo = SubmissionInfo
  { subStudent :: Student,
    subId :: SubmissionId,
    subTestSuite :: Assignment
  }
  deriving (Show, Eq, Ord)