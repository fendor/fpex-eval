module Fpex.Eval.Types where


import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           System.FilePath
import qualified Data.Text                     as T

import           Fpex.Course.Types

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

data TestCaseReport = TestCaseReport
    { testCaseReportLabel :: T.Text
    , testCaseReportResult :: TestCaseResult
    , testCaseReportTimeNs :: Integer
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TestGroupResults = TestGroupResults
    { testGroupReports :: [TestCaseReport]
    , testGroupPoints :: Points
    , testGroupResultProps :: TestGroupProps
    }
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

maxScore :: TestSuiteResults -> Points
maxScore TestSuiteResults {..} = sum (map (upperBound . testGroupResultProps) testGroupResults)

newtype Timeout = Timeout { getTimeout :: Float }
    deriving (Show, Generic)
    deriving newtype (Eq, Num, Ord)

newtype SubmissionId = SubmissionId { getSubmissionId :: Int }
    deriving (Show, Generic)
    deriving newtype (Eq, Num, Ord)

-- | Filename of the submission file
studentSourceFile :: Course -> T.Text -> Student -> FilePath
studentSourceFile course suiteName student =
    studentDir course student </> T.unpack suiteName <.> "hs"

assignmentCollectDir :: SubmissionId -> T.Text -> FilePath
assignmentCollectDir sid suiteName =
    (  T.unpack suiteName
    <> "-"
    <> show (getSubmissionId sid)
    )

assignmentCollectStudentDir
    :: SubmissionId -> T.Text -> Student -> FilePath
assignmentCollectStudentDir sid suiteName student =
    assignmentCollectDir sid suiteName </> T.unpack (studentId student)

assignmentCollectStudentFile :: SubmissionId -> T.Text -> Student -> FilePath
assignmentCollectStudentFile sid suiteName student =
    assignmentCollectStudentDir sid suiteName student
        </> T.unpack suiteName <.> "hs"

reportSourceJsonFile :: SubmissionId -> T.Text -> Student -> FilePath
reportSourceJsonFile sid suiteName student =
    assignmentCollectStudentDir sid suiteName student </> "report.json"

reportPublishFile :: SubmissionId -> Course -> T.Text -> Student -> FilePath
reportPublishFile sid course suiteName student =
    studentDir course student </> T.unpack suiteName <.> ("out_" <> show (getSubmissionId sid))

