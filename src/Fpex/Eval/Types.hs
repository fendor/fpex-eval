module Fpex.Eval.Types where

import           System.FilePath
import qualified Data.Text                     as T
import           Data.Maybe                     ( fromMaybe )
import           Data.List.Extra                ( stripSuffix )
import           GHC.Generics                   ( Generic )

import           Fpex.Course.Types


newtype Timeout = Timeout { getTimeout :: Float }
    deriving (Show, Generic)
    deriving newtype (Eq, Num, Ord)

newtype SubmissionId = SubmissionId { getSubmissionId :: Int }
    deriving (Show, Generic)
    deriving newtype (Eq, Num, Ord)

-- filename of the submission file
studentSourceFile :: Course -> String -> Student -> FilePath
studentSourceFile course assignmentFile student =
    studentDir course student </> assignmentFile

assignmentCollectDir :: SubmissionId -> Course -> String -> FilePath
assignmentCollectDir sid course assignmentName =
    courseAdminDir course
        </> (  fromMaybe assignmentName (stripSuffix ".hs" assignmentName)
            <> "-"
            <> show (getSubmissionId sid)
            )

assignmentCollectStudentDir
    :: SubmissionId -> Course -> String -> Student -> FilePath
assignmentCollectStudentDir sid course assignmentName student =
    assignmentCollectDir sid course assignmentName </> T.unpack (matrNr student)

assignmentCollectFile :: SubmissionId -> Course -> String -> Student -> FilePath
assignmentCollectFile sid course assignmentFile student =
    assignmentCollectStudentDir sid course assignmentFile student
        </> assignmentFile

reportSourceJsonFile :: SubmissionId -> Course -> String -> Student -> FilePath
reportSourceJsonFile sid course testSuite student =
    assignmentCollectStudentDir sid course testSuite student </> "report.json"

reportPublishFile :: SubmissionId -> Course -> String -> Student -> FilePath
reportPublishFile sid course assignmentName student =
    studentDir course student </> assignmentName <.> ("out_" <> show (getSubmissionId sid))
