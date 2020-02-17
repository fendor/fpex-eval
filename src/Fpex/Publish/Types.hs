module Fpex.Publish.Types where 

import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
 

-- | Types mirroring the types from fpex-test-spec
-- For backwards and forward compatibility.
 
type Points = Int

data TestGroupProps = TestGroupProps
    { label :: T.Text
    , pointsPerTest :: !Points
    , penalty :: !Points
    , upperBound :: !Points
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ExpectedButGot = ExpectedButGot T.Text T.Text
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestSuiteResults = TestSuiteResults
    { testGroupResults :: [TestGroupResults]
    , testSuitePoints :: Points
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestCaseResult
    = TestCaseResultOk
    | TestCaseResultExpectedButGot ExpectedButGot
    | TestCaseResultException T.Text
    | TestCaseResultTimeout
    | TestCaseResultNotSubmitted
    | TestCaseResultCompileFail
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestGroupResults = TestGroupResults
    { testCaseResults :: [(T.Text, TestCaseResult)]
    , testGroupPoints :: Points
    , testGroupResultProps :: TestGroupProps
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

