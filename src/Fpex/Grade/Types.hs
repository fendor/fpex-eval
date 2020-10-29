module Fpex.Grade.Types where

import Fpex.Course.Types

data RunnerInfo = RunnerInfo
  { runnerInfoTimeout :: Timeout,
    runnerInfoReportOutput :: FilePath,
    runnerInfoStudentSubmission :: StudentSubmission
  }
  deriving (Show, Eq, Ord)
