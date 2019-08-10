module Course where

import qualified Test.Tasty
import           Test.Tasty.Hspec
import           Fpex.Course.Types
import qualified Data.Text                     as T
import           Control.Monad                  ( forM_ )
import           Data.Aeson                     (decodeFileStrict')

import           Fpex.Course.Types

courseEmpty :: Course
courseEmpty =
    Course
        { courseName = "courseEmpty"
        , courseHomeDir = "testdata/courseEmpty"
        , courseStudents = []
        , courseGroups = []
        }

spec :: Spec
spec =
    describe "validate testdata course" $
        it "course1.json valid" $ do
            json <- decodeFileStrict' "testdata/courseEmpty.json"
            json `shouldBe` (Just courseEmpty)
