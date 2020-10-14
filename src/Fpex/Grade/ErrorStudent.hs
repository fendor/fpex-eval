module Fpex.Grade.ErrorStudent where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Grade
import Polysemy

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

prettyRunnerError :: SubmissionInfo -> RunnerError -> Sem r T.Text
prettyRunnerError _sinfo = \case
  RunnerInternalError msg serr ->
    pure $
      T.unlines $
        [ "Failed to execute neutral student",
          msg,
          "Stderr: ",
          T.pack $ LBS.unpack serr
        ]
  FailedToDecodeJsonResult msg ->
    pure $
      T.unlines
        ["Failed to decode the json result: ", T.pack msg]
  NoSubmission ->
    pure "Main.hs:Grade (NoSubmission) Invariant violated, can not be generated here."
