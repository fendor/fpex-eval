module Fpex.Collect where

import           System.FilePath
import           System.Directory
import           System.IO
import qualified Data.ByteString               as BS
import           Control.Monad.Extra            ( whenM )

import           Fpex.Course.Types
import           Fpex.Eval.Types


prepareSubmissionFolder :: SubmissionId -> Course -> String -> IO ()
prepareSubmissionFolder sid course testSuite = do

    let targetDir = assignmentCollectDir sid course testSuite
    createDirectoryIfMissing True targetDir

    -- copy assignment main
    let assignmentDir   = assignmentCollectDir sid course testSuite
    let testSuiteTarget = assignmentDir </> "Main.hs"

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

        createDirectoryIfMissing True targetDir

        -- copy submission file
        putStrLn $ "copy " <> sourceFile <> " to " <> targetFile
        copyFile sourceFile targetFile

    return ()
