module Fpex.Eval.Types where


import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           System.FilePath
import qualified Data.Text                     as T
import           Data.Maybe                     ( fromMaybe )
import           Data.List.Extra                ( stripSuffix )

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
studentSourceFile :: Course -> String -> Student -> FilePath
studentSourceFile course assignmentFile student =
    studentDir course student </> assignmentFile

assignmentCollectDir :: SubmissionId -> String -> FilePath
assignmentCollectDir sid assignmentName =
    (  fromMaybe assignmentName (stripSuffix ".hs" assignmentName)
    <> "-"
    <> show (getSubmissionId sid)
    )

assignmentCollectStudentDir
    :: SubmissionId -> String -> Student -> FilePath
assignmentCollectStudentDir sid assignmentName student =
    assignmentCollectDir sid assignmentName </> T.unpack (studentId student)

assignmentCollectStudentFile :: SubmissionId -> String -> Student -> FilePath
assignmentCollectStudentFile sid assignmentFile student =
    assignmentCollectStudentDir sid assignmentFile student
        </> assignmentFile

reportSourceJsonFile :: SubmissionId -> String -> Student -> FilePath
reportSourceJsonFile sid testSuite student =
    assignmentCollectStudentDir sid testSuite student </> "report.json"

reportPublishFile :: SubmissionId -> Course -> String -> Student -> FilePath
reportPublishFile sid course assignmentName student =
    studentDir course student </> assignmentName <.> ("out_" <> show (getSubmissionId sid))

