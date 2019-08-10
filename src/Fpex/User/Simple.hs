module Fpex.User.Simple where

import qualified Data.Text                     as T
import           Fpex.User.Types
import           System.Process                 ( CreateProcess
                                                , proc
                                                , readCreateProcessWithExitCode
                                                )
import           System.Exit                    ( ExitCode(..) )
import           Polysemy
import           Polysemy.Error
import Fpex.User.Effect

-- | Create a user with the given group and assign a random password to it.
-- If one of the commands fails, the user is deleted again and an error is returned.
createNewUser
    :: (Members '[UserManagement, PasswordGenerator, Error UserError] r)
    => Username
    -> UserGroup
    -> Sem r Password
createNewUser user group = do
    password                <- generatePassword

    (exitCodeUserCmd, _, _) <- createUser user group
    case exitCodeUserCmd of
        ExitFailure _ -> throw CreationFailed
        ExitSuccess   -> return ()

    (exitCodePassword, _, _) <- setPassword user password
    case exitCodePassword of
        ExitFailure _ -> do
            deleteUser user
            throw PasswordNotSet
        ExitSuccess -> return password

