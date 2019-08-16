module Fpex.Course.DirSetup where

import           System.Directory
import           Control.Monad                            ( forM_ )

import           Fpex.Course.Types

dirSetup :: Course -> IO ()
dirSetup course@Course {..} = do
    -- create course directory
    createDirectoryIfMissing True courseRootDir

    -- create admin-dir
    createDirectoryIfMissing False $ courseAdminDir course

    -- create directories of students
    forM_ courseStudents $ createDirectoryIfMissing False . studentDir course
