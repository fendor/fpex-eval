
module Fpex.Eval.Effect where

import           Polysemy
import           Polysemy.State
import           Polysemy.Reader
import           Polysemy.Internal
import qualified Data.Text                     as T
import           Data.Maybe                     ( isNothing )
import           Control.Monad
import           Control.Monad.Extra            ( whenJust )

import           System.Process
import           System.Exit                    ( ExitCode(..) )
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

runSessionGhciGrade
    :: Members '[Reader Timeout, Embed IO, State (Maybe ProcessState)] r
    => Sem (Grade : r) a
    -> Sem r a
runSessionGhciGrade = interpret $ \case
    RunTestCase fp testCase -> do
        timeoutTime <- ask
        procState   <- getGhciProcess fp
        embed (timeout (seconds timeoutTime) $ runTestCase procState testCase)
            >>= \case
                    Nothing -> do
                        embed $ stopGhciProcess procState
                        put @(Maybe ProcessState) Nothing
                        return TestCaseTimeout
                    Just testResult -> return testResult

runHugsGrade
    :: Members '[Reader Timeout, Embed IO] r => Sem (Grade : r) a -> Sem r a
runHugsGrade = interpret $ \case
    RunTestCase fp TestCase { query } -> do
        timeoutSec           <- ask @Timeout
        (exitCode, stout, _) <- embed $ readProcessWithExitCode
            "timeout"
            [show $ getTimeout timeoutSec, "hugs", "-p" <> T.unpack prompt, fp]
            (T.unpack query)
        case exitCode of
            ExitFailure 124 -> return TestCaseTimeout
            ExitFailure _   -> return TestCaseCompilefail
            ExitSuccess     -> do
                let actualOutput = T.strip $ T.pack stout
                return $ parseHugsOutput actualOutput
  where
    prompt :: T.Text
    prompt = ":a%a:"

    extractTermBetween :: T.Text -> T.Text -> T.Text
    extractTermBetween ctx input =
        T.strip
            . fst
            . T.breakOn prompt
            . T.drop (T.length prompt)
            . snd
            $ T.breakOn ctx input

    parseHugsOutput :: T.Text -> TestCaseResult
    parseHugsOutput input =
        let parsedResult = extractTermBetween prompt input
        in  case parsedResult of
                "ERROR - Control stack overflow" -> TestCaseTimeout
                x | T.isPrefixOf "ERROR" x -> TestCaseCompilefail
                x | T.isInfixOf "Program error" x -> TestRunTimeException
                _                          -> TestCaseRun $ TestRun parsedResult

runGrade
    :: (Members '[Reader Timeout, Embed IO] r)
    => GradeRunner
    -> Sem (Grade : State (Maybe ProcessState) : r) a
    -> Sem r a
runGrade gradeRunner act = do
    (s, res) <- runState @(Maybe ProcessState) Nothing $ case gradeRunner of
        Hugs      -> runHugsGrade act
        Ghci      -> runGhciGrade act
        SavedGhci -> runSessionGhciGrade act

    embed $ whenJust s stopGhciProcess
    return res


runGhciGrade
    :: Members '[Reader Timeout, Embed IO] r => Sem (Grade : r) a -> Sem r a
runGhciGrade = interpret $ \case
    RunTestCase fp TestCase { query } -> do
        timeoutSec           <- ask
        (exitCode, stout, _) <- embed $ readProcessWithExitCode
            "timeout"
            [show $ getTimeout timeoutSec, "ghci", fp, "-e", T.unpack query]
            []
        case exitCode of
            ExitFailure 124 -> return TestCaseTimeout
            ExitFailure _   -> return TestCaseCompilefail
            ExitSuccess     -> do
                let actualOutput = T.strip $ T.pack stout
                return . TestCaseRun $ TestRun actualOutput

runTestCase :: ProcessState -> TestCase -> IO TestCaseResult
runTestCase s TestCase { query } = do
    let (stin, stout, _, _) = ghciProcess s
    hPutStrLn stin (T.unpack query)
    output <- hGetLine stout
    return (TestCaseRun TestRun { actualOutput = T.pack output })

stopGhciProcess :: ProcessState -> IO ()
stopGhciProcess ProcessState {..} = do
    let procInfo@(stin, _, _, p) = ghciProcess
    hPutStrLn stin ":q"
    cleanupProcess $ wrap procInfo
    e <- getProcessExitCode p
    when (isNothing e) $ terminateProcess p
  where
    wrap
        :: (Handle, Handle, Handle, ProcessHandle)
        -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    wrap (o, i, e, p') = (Just o, Just i, Just e, p')

createGhciProcess
    :: FilePath -> FilePath -> IO (Handle, Handle, Handle, ProcessHandle)
createGhciProcess process fp = do
    (Just stin, Just stout, Just sterr, procHandle) <- createProcess
        (proc process ["-v0", fp]) { std_out = CreatePipe
                                   , std_in  = CreatePipe
                                   , std_err = CreatePipe
                                   }
    hSetBuffering stin  LineBuffering
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
                (handleStdin, handleStdout, handleStderr, procHandle) -> do
                    let procState = ProcessState
                            { assignment  = fp
                            , ghciProcess = ( handleStdin
                                            , handleStdout
                                            , handleStderr
                                            , procHandle
                                            )
                            }

                    put @(Maybe ProcessState) (Just procState)
                    return procState
        Just procState -> if assignment procState == fp
            then return procState
            else do
                embed $ stopGhciProcess procState
                put @(Maybe ProcessState) Nothing
                procInfo <- embed $ createGhciProcess "ghci" fp
                case procInfo of
                    (handleStdin, handleStdout, handleStderr, procHandle) -> do
                        let newProcState = ProcessState
                                { assignment  = fp
                                , ghciProcess = ( handleStdin
                                                , handleStdout
                                                , handleStderr
                                                , procHandle
                                                )
                                }
                        put @(Maybe ProcessState) (Just newProcState)
                        return newProcState

runStudentData :: Members '[Embed IO, Reader SubmissionId] r => Sem (StudentData : r) a -> Sem r a
runStudentData = interpret $ \case
    GetStudentSubmission course testSuite student -> do
        sid <- ask
        let sourceFile = assignmentCollectFile sid course testSuite student
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
