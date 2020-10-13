module Fpex.Grade.ErrorStudent where

import Fpex.Course.Types

errorStudent :: Student
errorStudent = Student "errorStudent"

isErrorStudent :: Student -> Bool
isErrorStudent = (== errorStudent)

errorStudentSubmissionInfo :: SubmissionId -> SubmissionName -> SubmissionInfo
errorStudentSubmissionInfo sid submissionName =
  SubmissionInfo
    { subId = sid,
      subName = submissionName,
      subStudent = errorStudent
    }