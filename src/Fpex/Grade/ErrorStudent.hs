module Fpex.Grade.ErrorStudent where

import Fpex.Course.Types

errorStudent :: Student
errorStudent = Student "errorStudent"

isErrorStudent :: Student -> Bool
isErrorStudent = (== errorStudent)

errorStudentSubmissionInfo :: SubmissionId -> Assignment -> SubmissionInfo
errorStudentSubmissionInfo sid suiteName =
  SubmissionInfo
    { subId = sid,
      subTestSuite = suiteName,
      subStudent = errorStudent
    }