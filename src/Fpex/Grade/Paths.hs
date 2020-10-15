module Fpex.Grade.Paths where

import qualified Data.Text as T
import Fpex.Course.Types
import Polysemy
import Polysemy.Reader
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

reportTestSuiteFile ::
  SubmissionId ->
  SubmissionName ->
  StudentSubmission ->
  Student ->
  FilePath
reportTestSuiteFile sid suiteName studentSubmission student =
  assignmentCollectStudentDir' sid suiteName student
    </> testSuiteName sid studentSubmission

testSuiteName :: SubmissionId -> StudentSubmission -> FilePath
testSuiteName sid studentSubmission =
  ( dropExtension (getStudentSubmission studentSubmission)
      ++ "_TestSuite"
      ++ show (getSubmissionId sid) <.> "hs"
  )

reportFeedbackFile ::
  SubmissionId ->
  SubmissionName ->
  StudentSubmission ->
  Student ->
  FilePath
reportFeedbackFile sid suiteName studentSubmission student =
  assignmentCollectStudentDir' sid suiteName student
    </> reportName sid studentSubmission

reportName :: SubmissionId -> StudentSubmission -> String
reportName sid studentSubmission =
  getStudentSubmission studentSubmission
    <.> ("out_" <> show (getSubmissionId sid))

assignmentPath :: SubmissionName -> FilePath
assignmentPath (SubmissionName t) = T.unpack t

-- ----------------------------------------------------------------------------
-- Filepath utilities for accessing student directories
-- ----------------------------------------------------------------------------

-- | Filename of the submission file
studentSourceFile :: Course -> StudentSubmission -> Student -> FilePath
studentSourceFile course studentSubmission student =
  studentDir course student </> getStudentSubmission studentSubmission

reportPublishFile ::
  SubmissionId ->
  Course ->
  StudentSubmission ->
  Student ->
  FilePath
reportPublishFile sid course studentSubmission student =
  studentDir course student
    </> reportName sid studentSubmission

studentTestSuiteFile ::
  SubmissionId ->
  Course ->
  StudentSubmission ->
  Student ->
  FilePath
studentTestSuiteFile sid course studentSubmission student =
  studentDir course student
    </> testSuiteName sid studentSubmission
