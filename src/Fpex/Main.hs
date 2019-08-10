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
import           Fpex.Eval.Main
import           Fpex.Eval.Pretty              as Eval

defaultMain :: IO ()
defaultMain = do
    Options {..} <- execParser options
    -- TODO: error effect
    Just course@Course{..} <- decodeFileStrict' optionCourseFile :: IO (Maybe Course)
    print course

    case optionCommand of
        Grade CommandGrade {..} -> do
            Just testSuite <- decodeFileStrict' testSuiteFile
            let students = case optionStudent of
                   Just s -> [s]
                   Nothing -> courseStudents
            forM_ students $ \student@Student{..} -> do
                T.putStrLn $ "eval student " <> matrNr
                testReport <- evalStudent testSuite student
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
