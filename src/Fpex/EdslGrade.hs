module Fpex.EdslGrade where

import           System.Directory
import           System.FilePath
import qualified System.Process                as Proc
import qualified Data.Text                     as T
import           System.IO
import           System.Exit                    ( ExitCode(..) )
import           Control.Monad.Extra            ( whenM )
import qualified Data.ByteString               as BS
import           Data.Maybe                     ( fromMaybe )
import           Data.List.Extra                ( stripSuffix )

import           Fpex.Course.Types
import           Fpex.Eval.Types                ( SubmissionId
                                                , getSubmissionId
                                                )



prepareSubmissionFolder :: SubmissionId -> Course -> String -> IO ()
prepareSubmissionFolder sid course testSuite = do

    let targetDir = assignmentCollectDir sid course testSuite
    createDirectoryIfMissing True targetDir


    -- copy test spec lib and assignment main
    let assignmentDir     = assignmentCollectDir sid course testSuite
    let testSpecLibTarget = assignmentDir </> "TestSpecLib.hs"
    let testSuiteTarget   = assignmentDir </> "Main.hs"

    putStrLn "copy TestSpecLib.hs"
    copyFile "TestSpecLib.hs" testSpecLibTarget

    putStrLn $ "copy " <> testSuite
    copyFile testSuite testSuiteTarget

    return ()



collectSubmission :: SubmissionId -> Course -> String -> Student -> IO ()
collectSubmission sid course testSuite student = do

    let sourceFile = studentSourceFile course testSuite student
    let targetDir  = assignmentCollectStudentDir sid course testSuite student
    let targetFile = assignmentCollectFile sid course testSuite student

    whenM (doesFileExist sourceFile) $ do
        whenM (BS.isInfixOf "unsafePerformIO" <$> BS.readFile sourceFile)
            $  hPutStrLn stderr
            $  "Warning: `unsafePerformIO` in submission "
            <> sourceFile

        putStrLn $ "copy " <> sourceFile <> " to " <> targetFile

        createDirectoryIfMissing True targetDir
        copyFile sourceFile targetFile


    return ()

linkSubmission :: SubmissionId -> Course -> String -> Student -> IO ()
linkSubmission sid course testSuite student = do

    let assignmentDir      = assignmentCollectDir sid course testSuite
    let relativeStudentDir = T.unpack (matrNr student)


    putStrLn $ "compile Main for student " <> T.unpack (matrNr student)
    let procConfig = (Proc.proc
                         "ghc"
                         [ "Main.hs"
                         , "-i" <> relativeStudentDir
                         , "-outputdir"
                         , relativeStudentDir
                         , "-o"
                         , relativeStudentDir </> "Main"
                         ]
                     )
            { Proc.cwd = Just assignmentDir
            }
    (_, _, _, procHandle) <- Proc.createProcess procConfig
    ExitSuccess           <- Proc.waitForProcess procHandle

    return ()


runSubmission :: SubmissionId -> Course -> String -> Student -> IO ()
runSubmission sid course testSuite student = do

    let targetDir = assignmentCollectStudentDir sid course testSuite student
    let testMain  = targetDir </> "Main"

    putStrLn $ "run testsuite for student " <> T.unpack (matrNr student)


    let procConfig = (Proc.proc testMain [])
    (_, _, _, procHandle) <- Proc.createProcess procConfig
    ExitSuccess           <- Proc.waitForProcess procHandle

    return ()




-- filename of the submission file
studentSourceFile :: Course -> String -> Student -> FilePath
studentSourceFile course assignmentFile student =
    studentDir course student </> assignmentFile

assignmentCollectDir :: SubmissionId -> Course -> String -> FilePath
assignmentCollectDir sid course assignmentName =
    courseAdminDir course
        </> (  fromMaybe assignmentName (stripSuffix ".hs" assignmentName)
            <> "-"
            <> show (getSubmissionId sid)
            )

assignmentCollectStudentDir
    :: SubmissionId -> Course -> String -> Student -> FilePath
assignmentCollectStudentDir sid course assignmentName student =
    assignmentCollectDir sid course assignmentName </> T.unpack (matrNr student)

assignmentCollectFile :: SubmissionId -> Course -> String -> Student -> FilePath
assignmentCollectFile sid course assignmentFile student =
    assignmentCollectStudentDir sid course assignmentFile student
        </> assignmentFile
