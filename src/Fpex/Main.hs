module Fpex.Main where

import           Data.Function                  ( (&) )
import           Options.Applicative
import qualified Data.Text.Lazy                as L
import qualified Data.Text.IO                  as T
import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Aeson                     ( decodeFileStrict' )
import           Fpex.Options
import           Fpex.Course.Types
import qualified Fpex.User.Simple              as User
import qualified Fpex.User.Types               as User
import qualified Fpex.User.Effect              as User
import           Fpex.Eval.Main
import           Polysemy.Plugin                ( )
import           Polysemy                       ( runM )
import           Polysemy.Error                 ( runError )
import           Polysemy.IO                    ( embedToMonadIO )

defaultMain :: IO ()
defaultMain = do
    Options {..} <- execParser options
    -- TODO: error effect
    Just course  <- decodeFileStrict' optionCourseFile :: IO (Maybe Course)

    case optionCommand of
        Eval CommandGrade {..} -> do
            Just testSuite <- decodeFileStrict' testSuiteFile
            testReport     <- evalStudent testSuite student
            putStrLn . L.unpack . encodeToLazyText $ testReport

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
