module Course where

import           Test.Tasty.Hspec
import           Fpex.Course.Types
import           Data.Aeson                     ( decodeFileStrict' )

courseEmpty :: Course
courseEmpty = Course { courseName       = "courseEmpty"
                     , courseRootDir    = "testdata/courseEmpty"
                     , courseStudents   = []
                     , courseGroups     = []
                     , courseUserPrefix = "x"
                     }
course1 :: Course
course1 = Course
    { courseName       = "course1"
    , courseRootDir    = "testdata/course1"
    , courseStudents   = [ Student "1234567"
                         , Student "1000000"
                         , Student "1113330"
                         , Student "1711000"
                         , Student "1831000"
                         ]
    , courseGroups     = []
    , courseUserPrefix = "c1-"
    }

course2 :: Course
course2 = Course { courseName       = "course2"
                 , courseRootDir    = "testdata/course2"
                 , courseStudents   = [Student "1234567", Student "1000000"]
                 , courseGroups     = []
                 , courseUserPrefix = "c2-"
                 }

course3 :: Course
course3 = Course { courseName       = "course3"
                 , courseRootDir    = "testdata/course3"
                 , courseStudents   = [Student "1234567", Student "1000000"]
                 , courseGroups     = []
                 , courseUserPrefix = "c3-"
                 }

spec :: Spec
spec = describe "validate testdata course" $ do
    it "courseEmpty.json valid" $ do
        json <- decodeFileStrict' "testdata/courseEmpty.json"
        json `shouldBe` (Just courseEmpty)
    it "course1.json valid" $ do
        json <- decodeFileStrict' "testdata/course1.json"
        json `shouldBe` (Just course1)
    it "course2.json valid" $ do
        json <- decodeFileStrict' "testdata/course2.json"
        json `shouldBe` (Just course2)
    it "course3.json valid" $ do
        json <- decodeFileStrict' "testdata/course3.json"
        json `shouldBe` (Just course3)
