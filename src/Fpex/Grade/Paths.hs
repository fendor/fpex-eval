module Fpex.Grade.Paths where

import Polysemy
import Polysemy.Reader
import Fpex.Course.Types
import qualified Data.Text as T
import System.FilePath

-- ----------------------------------------------------------------------------
-- Monadic functions for extracting filepath information
-- ----------------------------------------------------------------------------

assignmentCollectStudentDir ::
  Member (Reader SubmissionInfo) r => Sem r FilePath
assignmentCollectStudentDir =
  assignmentCollectStudentDir' <$> asks subId <*> asks subName <*> asks subStudent


-- ----------------------------------------------------------------------------
-- Pure functions for extracting filepath information
-- ----------------------------------------------------------------------------

testSuiteMain :: Course -> FilePath
testSuiteMain Course {..} = courseRootDir </> "Main" <.> "hs"

studentDir :: Course -> Student -> FilePath
studentDir Course {..} Student {..} =
  courseRootDir </> T.unpack studentId

-- | Filename of the submission file
studentSourceFile :: Course -> StudentSubmission -> Student -> FilePath
studentSourceFile course studentSubmission student =
  studentDir course student </> getStudentSubmission studentSubmission

assignmentCollectDir :: SubmissionId -> SubmissionName -> FilePath
assignmentCollectDir sid suiteName =
  assignmentPath suiteName
    <> "-"
    <> show (getSubmissionId sid)

assignmentCollectStudentDir' ::
  SubmissionId -> SubmissionName -> Student -> FilePath
assignmentCollectStudentDir' sid suiteName student =
  assignmentCollectDir sid suiteName </> T.unpack (studentId student)

assignmentCollectStudentFile ::
  SubmissionId ->
  SubmissionName ->
  StudentSubmission ->
  Student ->
  FilePath
assignmentCollectStudentFile sid suiteName studentSubmission student =
  assignmentCollectStudentDir' sid suiteName student
    </> getStudentSubmission studentSubmission

reportSourceJsonFile ::
  SubmissionId ->
  SubmissionName ->
  Student ->
  FilePath
reportSourceJsonFile sid suiteName student =
  assignmentCollectStudentDir' sid suiteName student </> "report.json"

reportPublishFile ::
  SubmissionId ->
  Course ->
  SubmissionName ->
  Student ->
  FilePath
reportPublishFile sid course suiteName student =
  studentDir course student
    </> reportName sid suiteName

reportFeedbackFile ::
  SubmissionId ->
  SubmissionName ->
  Student ->
  FilePath
reportFeedbackFile sid suiteName student =
  assignmentCollectStudentDir' sid suiteName student
    </> reportName sid suiteName

reportName :: SubmissionId -> SubmissionName -> String
reportName sid suiteName =
  assignmentPath suiteName
    <.> ("out_" <> show (getSubmissionId sid))

assignmentPath :: SubmissionName -> FilePath
assignmentPath (SubmissionName t) = T.unpack t