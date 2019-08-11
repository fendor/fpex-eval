module Fpex.Main where

import           Data.Function                  ( (&) )
import qualified Data.Text.IO                  as T
import           Data.Aeson                     ( decodeFileStrict' )
import           Control.Monad                  ( forM_ )

import           Options.Applicative

import           Polysemy                       ( runM )
import           Polysemy.Error                 ( runError )
import           Polysemy.IO                    ( embedToMonadIO )

import           Fpex.Options
import           Fpex.Course.Types
import qualified Fpex.User.Simple              as User
import qualified Fpex.User.Types               as User
import qualified Fpex.User.Effect              as User
import           Fpex.Eval.Main                as Eval
import           Fpex.Eval.Pretty              as Eval
import           Fpex.Course.DirSetup          as Setup
import           Fpex.Collect                  as Collect

defaultMain :: IO ()
defaultMain = do
    Options {..} <- execParser options
    -- TODO: error effect
    Just course@Course{..} <- decodeFileStrict' optionCourseFile
    print course
    let students = maybe courseStudents pure optionStudent

    case optionCommand of
        Grade CommandGrade {..} -> do
            Just testSuite <- decodeFileStrict' testSuiteFile
            forM_ students $ \student@Student{..} -> do
                T.putStrLn $ "grade student " <> matrNr
                testReport <- Eval.evalStudent testSuite student
                T.putStrLn $ prettyTestReport testReport
                putStrLn ""

        User UserManagementCommand {..} ->

            User.createNewUser username userGroup
                &   User.runUserManagement
                &   User.runPasswordGenerator
                &   runError
                &   embedToMonadIO
                &   runM
                >>= \case
                        Left  err                        -> print err
                        Right User.Password { password } -> T.putStrLn password


        Setup ->
            Setup.dirSetup course
        Collect CollectCommand {collectTestSuiteFile} -> do
            Just testSuite <- decodeFileStrict' collectTestSuiteFile
            forM_ students $ Collect.collectAssignment course testSuite
