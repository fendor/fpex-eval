module Fpex.Main where

import           Options.Applicative
import qualified Data.Text.Lazy                as L
import qualified Data.Text.IO                  as T
import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Aeson                     ( decodeFileStrict' )
import           Fpex.Options
import qualified Fpex.User.Simple              as User
import qualified Fpex.User.Types               as User
import           Fpex.Eval.Main

defaultMain :: IO ()
defaultMain = execParser options >>= \case
    Eval CommandGrade {..} -> do
        Just testSuite <- decodeFileStrict' testSuiteFile
        testReport     <- evalStudent testSuite student
        putStrLn . L.unpack . encodeToLazyText $ testReport

    User UserManagementCommand {..} ->

        User.createUser username userGroup >>= \case
            Left err -> print err

            Right User.Password { password } ->
                T.putStrLn password
