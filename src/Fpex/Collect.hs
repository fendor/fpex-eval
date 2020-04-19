module Fpex.Collect where

import           System.FilePath
import           System.Directory
import           System.IO
import qualified Data.ByteString               as BS
import           Control.Monad.Extra            ( whenM )

import           Fpex.Course.Types
import           Fpex.Eval.Types


prepareSubmissionFolder :: SubmissionId -> FilePath -> IO ()
prepareSubmissionFolder sid testSuite = do

    let targetDir = assignmentCollectDir sid testSuite
    createDirectoryIfMissing True targetDir

    -- copy assignment main
    let testSuiteTarget = targetDir </> "Main.hs"
    putStrLn $ "copy " <> testSuite
    copyFile testSuite testSuiteTarget

    -- copy test-spec file
    copyFile "TestSpec.hs" (targetDir </> "TestSpec.hs")

    return ()

collectSubmission :: SubmissionId -> Course -> String -> Student -> IO (Either FailureReason FilePath)
collectSubmission sid course testSuite student = do
    let sourceFile = studentSourceFile course testSuite student
    let targetDir  = assignmentCollectStudentDir sid testSuite student
    let targetFile = assignmentCollectStudentFile sid testSuite student
    -- create submission dir even if there is no submission
    createDirectoryIfMissing True targetDir

    -- copy submission file if it exists
    fileExists <- doesFileExist sourceFile
    if fileExists
        then do
            whenM (BS.isInfixOf "unsafePerformIO" <$> BS.readFile sourceFile)
                $  hPutStrLn stderr
                $  "Warning: `unsafePerformIO` in submission "
                <> sourceFile
            putStrLn $ "copy " <> sourceFile <> " to " <> targetFile
            copyFile sourceFile targetFile
            return (Right targetFile)
        else do
            return (Left NoSubmission)
