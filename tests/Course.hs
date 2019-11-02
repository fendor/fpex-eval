module Course where

import           Test.Tasty.Hspec
import           Fpex.Course.Types
import           Data.Aeson                     (decodeFileStrict')

courseEmpty :: Course
courseEmpty =
    Course
        { courseName = "courseEmpty"
        , courseRootDir = "testdata/courseEmpty"
        , courseStudents = []
        , courseGroups = []
        , courseUserPrefix = "x"
        }

spec :: Spec
spec =
    describe "validate testdata course" $
        it "course1.json valid" $ do
            json <- decodeFileStrict' "testdata/courseEmpty.json"
            json `shouldBe` (Just courseEmpty)
