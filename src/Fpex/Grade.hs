module Fpex.Grade where

import qualified System.Process                as Proc
import qualified Data.Text                     as T
import           System.Exit                    ( ExitCode(..) )

import           Fpex.Course.Types
import           Fpex.Eval.Types


runSubmission :: SubmissionId -> Course -> String -> Student -> IO ()
runSubmission sid course testSuite student = do
    let targetDir = assignmentCollectStudentDir sid course testSuite student

    putStrLn $ "run testsuite for student " <> T.unpack (matrNr student)
    let procConfig = (Proc.proc "ghci" ["../Main.hs", "-e", "main"])
            { Proc.cwd = Just targetDir
            }
    (_, _, _, procHandle) <- Proc.createProcess procConfig
    ExitSuccess           <- Proc.waitForProcess procHandle

    return ()

