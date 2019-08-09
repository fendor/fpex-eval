module Fpex.User.Simple where

import qualified Data.Text                     as T
import           Fpex.User.Types
import           System.Process                 ( CreateProcess
                                                , proc
                                                , readCreateProcessWithExitCode
                                                )
import           System.Exit                    ( ExitCode(..) )


-- | Create a user with the given group and assign a random password to it.
-- If one of the commands fails, the user is deleted again and an error is returned.
createUser :: Username -> Group -> IO (Either UserError Password)
createUser user group = do
    -- TODO: Use mtl
    pwd@Password { getPassword = password } <- newRandomPassword
    let createUserCmd  = buildCreateUserCmd user group
    let setPasswordCmd = buildSetPasswordCmd user
    (exitCodeUserCmd, _, _) <- readCreateProcessWithExitCode createUserCmd ""
    case exitCodeUserCmd of
        ExitFailure _ -> return $ Left CreationFailed
        ExitSuccess   -> do
            (exitCodePasswordCmd, _, _) <- readCreateProcessWithExitCode
                setPasswordCmd
                (unlines $ map T.unpack [password, password])
            case exitCodePasswordCmd of
                ExitFailure _ -> do
                    _ <- readCreateProcessWithExitCode
                        (buildDeleteUserCmd user)
                        ""
                    return $ Left PasswordNotSet
                ExitSuccess -> return $ Right pwd

-- | Uses pwgen utility to generate a random password.
newRandomPassword :: IO Password
newRandomPassword = return "secret"


buildCreateUserCmd :: Username -> Group -> CreateProcess
buildCreateUserCmd Username { getUsername = username } Group { getGroup = group, getPrefix = prefix }
    = proc "useradd"
        $  ["-m", T.unpack username, "-g", T.unpack group]
        ++ maybe [] (\p -> ["-b", T.unpack p]) prefix

buildSetPasswordCmd :: Username -> CreateProcess
buildSetPasswordCmd Username { getUsername = username } =
    proc "passwd" [T.unpack username]

buildDeleteUserCmd :: Username -> CreateProcess
buildDeleteUserCmd Username { getUsername = username } =
    proc "userdel" ["-r", T.unpack username]
