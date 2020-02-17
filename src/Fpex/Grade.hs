module Fpex.Grade where

import qualified System.Process                as Proc
import qualified Data.Text                     as T
import           System.Exit                    ( ExitCode(..) )
import           System.Directory
import           Control.Monad.Extra            ( unlessM )

import           Polysemy
import           Polysemy.Error
import           Fpex.Course.Types
import           Fpex.Eval.Types

data RunnerError
    = RunnerFailedToCompile
    | NoSubmission
    deriving (Show, Eq, Read, Ord)


runGradeError :: Sem (Error RunnerError ': r) a -> Sem r (Either RunnerError a)
runGradeError = runError

runSubmission :: (Member (Embed IO) r, Member (Error RunnerError) r) => SubmissionId -> Course -> String -> Student -> Sem r ()
runSubmission sid course testSuite student = do
    let targetDir = assignmentCollectStudentDir sid course testSuite student
    let targetFile = assignmentCollectStudentFile sid course testSuite student

    unlessM (embed $ doesFileExist targetFile) $ throw NoSubmission
    
    procRes <- embed $ do
        putStrLn $ "run testsuite for student " <> T.unpack (matrNr student)
        let
            procConfig =
                (Proc.proc "ghci"
                           ["../Main.hs", "-i", "-i.", "-i..", "-e", "Main.main"]
                    )
                    { Proc.cwd = Just targetDir
                    }
        (_, _, _, procHandle) <- Proc.createProcess procConfig
        Proc.waitForProcess procHandle 
        
    case procRes of 
        ExitSuccess -> return ()
        ExitFailure _ -> throw RunnerFailedToCompile
