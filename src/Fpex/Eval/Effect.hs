
module Fpex.Eval.Effect where

import           Polysemy
import           Polysemy.State
import           Polysemy.Internal
import qualified Data.Text                     as T

import           System.Process
import           System.IO
import           System.Directory               ( doesFileExist )
import           System.Timeout                 ( timeout )

import           Fpex.Course.Types
import           Fpex.Eval.Types

data ProcessState = ProcessState
    { assignment :: FilePath
    , ghciProcess :: (Handle, Handle, Handle, ProcessHandle)
    }


data Grade m a where
    RunTestCase ::FilePath -> TestCase -> Grade m TestCaseResult

data StudentData m a where
    GetStudentSubmission ::Course -> TestSuite -> Student -> StudentData m (Maybe FilePath)

runGrade :: Members [Embed IO, State (Maybe ProcessState)] r => Sem (Grade : r) a -> Sem r a
runGrade = interpret $ \case
    RunTestCase fp testCase -> do
        procState <- getGhciProcess fp
        embed (timeout (seconds 5) $ runTestCase (ghciProcess procState) testCase)
            >>= \case
            Nothing -> do
                embed $ stopGhciProcess (ghciProcess procState)
                put @(Maybe ProcessState) Nothing
                return TestCaseTimeout
            Just testResult  -> return testResult

    where
        seconds = (* (1_000_000 :: Int))

runTestCase
    :: (Handle, Handle, Handle, ProcessHandle) -> TestCase -> IO TestCaseResult
runTestCase (stin, stout, _, _) TestCase { query } = do
    hPutStrLn stin (T.unpack query)
    output <- hGetLine stout
    return (TestCaseRun TestRun { actualOutput = T.pack output })

stopGhciProcess :: (Handle, Handle, Handle, ProcessHandle) -> IO ()
stopGhciProcess = cleanupProcess . wrap
  where
    wrap
        :: (Handle, Handle, Handle, ProcessHandle)
        -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    wrap (o, i, e, p) = (Just o, Just i, Just e, p)

createGhciProcess
    :: FilePath
    -> FilePath
    -> IO (Handle, Handle, Handle, ProcessHandle)
createGhciProcess process fp = do
    (Just stin, Just stout, Just sterr, procHandle) <- createProcess (proc process ["-v0", fp])
        { std_out       = CreatePipe
        , std_in        = CreatePipe
        , std_err       = CreatePipe
        }
    hSetBuffering stin LineBuffering
    hSetBuffering stout LineBuffering
    hSetBuffering sterr LineBuffering
    return (stin, stout, sterr, procHandle)

getGhciProcess
    :: (Members '[Embed IO, State (Maybe ProcessState)] r)
    => FilePath
    -> Sem r ProcessState
getGhciProcess fp = do
    stateM <- get
    case stateM of
        Nothing -> do
            procInfo <- embed $ createGhciProcess "ghci" fp
            case procInfo of
                (handleStdin, handleStdout, handleStderr, procHandle) ->
                    do
                        let procState =  ProcessState
                                { assignment  = fp
                                , ghciProcess =
                                        ( handleStdin
                                        , handleStdout
                                        , handleStderr
                                        , procHandle
                                        )
                                }

                        put @(Maybe ProcessState) (Just procState)
                        return procState
        Just procState ->
            if assignment procState == fp
                then return procState
                else do
                    embed . stopGhciProcess $ ghciProcess procState
                    put @(Maybe ProcessState) Nothing
                    procInfo <- embed $ createGhciProcess "ghci" fp
                    case procInfo of
                        (handleStdin, handleStdout, handleStderr, procHandle) ->
                            do
                                let newProcState =  ProcessState
                                        { assignment  = fp
                                        , ghciProcess =
                                                ( handleStdin
                                                , handleStdout
                                                , handleStderr
                                                , procHandle
                                                )
                                        }
                                put @(Maybe ProcessState) (Just newProcState)
                                return newProcState



runStudentData :: Member (Embed IO) r => Sem (StudentData : r) a -> Sem r a
runStudentData = interpret $ \case
    GetStudentSubmission course testSuite student -> do
        let sourceFile = assignmentCollectFile course testSuite student
        embed (doesFileExist sourceFile) >>= \case
            True  -> pure $ Just sourceFile
            False -> pure Nothing

gradeTestCase :: Member Grade r => FilePath -> TestCase -> Sem r TestCaseResult
gradeTestCase fp testCase = send (RunTestCase fp testCase)

getStudentSubmission
    :: Member StudentData r
    => Course
    -> TestSuite
    -> Student
    -> Sem r (Maybe FilePath)
getStudentSubmission course suite student =
    send (GetStudentSubmission course suite student)
