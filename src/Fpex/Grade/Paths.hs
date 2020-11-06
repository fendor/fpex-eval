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

reportTestSuiteFile ::
  Members [Reader SubmissionInfo, Reader StudentSubmission] r => Sem r FilePath
reportTestSuiteFile =
  reportTestSuiteFile' <$> asks subId <*> asks subName <*> ask <*> asks subStudent

-- ----------------------------------------------------------------------------
-- Pure functions for extracting filepath information
-- ----------------------------------------------------------------------------

testSuiteMain :: SubmissionId -> SubmissionName -> FilePath
testSuiteMain sid suiteName = assignmentCollectDir sid suiteName </> "Main" <.> "hs"

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

reportTestSuiteFile' ::
  SubmissionId ->
  SubmissionName ->
  StudentSubmission ->
  Student ->
  FilePath
reportTestSuiteFile' sid suiteName studentSubmission student =
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

