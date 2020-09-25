module Fpex.Grade.ErrorStudent where

import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade.Types

errorStudent :: Student
errorStudent = Student "errorStudent"

isErrorStudent :: Student -> Bool
isErrorStudent = (== errorStudent)

errorStudentSubmissionInfo :: SubmissionId -> T.Text -> SubmissionInfo
errorStudentSubmissionInfo sid suiteName =
  SubmissionInfo
    { subId = sid,
      subTestSuite = suiteName,
      subStudent = errorStudent
    }