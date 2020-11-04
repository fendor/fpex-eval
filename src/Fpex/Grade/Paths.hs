module Fpex.Grade.Paths where

import Data.Maybe (fromMaybe)
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

-- ----------------------------------------------------------------------------
-- Filepath utilities for accessing student directories
-- ----------------------------------------------------------------------------

-- | Root directory of the real student
studentDir :: Course -> Student -> FilePath
studentDir Course {..} Student {..} =
  courseRootDir </> T.unpack studentId

-- | Absolute path to the real student's sub-directory to which feedback can be delivered.
studentSubDir :: Course -> Student -> FilePath
studentSubDir Course {..} Student {..} =
  normalise $
    studentDir Course {..} Student {..}
      </> fromMaybe "." courseStudentSubDir

-- | Filename of the submission file
studentSourceFile :: Course -> StudentSubmission -> Student -> FilePath
studentSourceFile course studentSubmission student =
  studentDir course student </> getStudentSubmission studentSubmission

-- | Location of the grading result.
reportPublishFile ::
  SubmissionId ->
  Course ->
  StudentSubmission ->
  Student ->
  FilePath
reportPublishFile sid course studentSubmission student =
  studentSubDir course student
    </> reportName sid studentSubmission

studentTestSuiteFile :: Members [Reader Course, Reader SubmissionInfo, Reader StudentSubmission] r => Sem r FilePath
studentTestSuiteFile =
  studentTestSuiteFile' <$> ask <*> asks subId <*> ask <*> asks subStudent

-- | Location of the test-suite delivered to the student.
studentTestSuiteFile' ::
  Course ->
  SubmissionId ->
  StudentSubmission ->
  Student ->
  FilePath
studentTestSuiteFile' course sid studentSubmission student =
  studentSubDir course student
    </> testSuiteName sid studentSubmission
