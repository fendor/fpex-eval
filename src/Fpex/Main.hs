module Fpex.Main where

import           Options.Applicative
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.IO                  as T
import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Aeson                     ( decodeFileStrict' )
import           Control.Monad                  ( forM_ )

import           Fpex.Options
import           Fpex.Course.Types
import qualified Fpex.User.Simple              as User
import qualified Fpex.User.Types               as User
import           Fpex.Eval.Main                as Eval
import           Fpex.Eval.Pretty              as Eval

defaultMain :: IO ()
defaultMain = do
    Options { .. } <- execParser options
    -- TODO: error effect
    Just course@Course{..} <- decodeFileStrict' optionCourseFile :: IO (Maybe Course)
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

            User.createUser username userGroup >>= \case
                Left err -> print err

                Right User.Password { password } ->
                    T.putStrLn password
