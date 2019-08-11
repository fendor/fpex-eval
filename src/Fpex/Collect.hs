module Fpex.Collect where

import qualified Data.Text                      as T
import           System.Directory
import           System.FilePath
import           Control.Monad.Extra (whenM)

import Fpex.Course.Types
import Fpex.Eval.Types

-- | collect assignment of single student
collectAssignment :: Course -> TestSuite -> Student -> IO ()
collectAssignment course TestSuite {assignmentName} student@Student{matrNr} = do

    let sourceFile = studentDir course student </> T.unpack assignmentName <.> "hs"
    let targetDir = courseAdminDir course </> T.unpack assignmentName
    let targetFile = targetDir </> T.unpack matrNr <.> "hs"

    createDirectoryIfMissing False $ targetDir


    whenM (doesFileExist sourceFile) $ copyFile sourceFile targetFile
