module Fpex.User.Effect where


import qualified Data.Text                     as T
import           Control.Monad.IO.Class
import           Polysemy
import           Polysemy.Internal
import           System.Process                 ( CreateProcess
                                                , proc
                                                , readCreateProcessWithExitCode
                                                )
import           System.Exit                    ( ExitCode )


import           Fpex.User.Types

runUserManagement
    :: Member (Embed IO) r => Sem (UserManagement : r) a -> Sem r a
runUserManagement = interpret $ \case
    CreateUser  user group    -> createUserCmd user group
    SetPassword user password -> setPasswordCmd user password
    DeleteUser user           -> deleteUserCmd user

runPasswordGenerator
    :: Member (Embed IO) r => Sem (PasswordGenerator : r) a -> Sem r a
runPasswordGenerator = interpret $ \case
    GeneratePassword -> newRandomPassword

data UserManagement m a where
    CreateUser ::Username -> UserGroup -> UserManagement m (ExitCode, String, String)
    SetPassword ::Username -> Password -> UserManagement m (ExitCode, String, String)
    DeleteUser ::Username ->  UserManagement m ()

data PasswordGenerator m a where
    GeneratePassword ::PasswordGenerator m Password

generatePassword :: Member PasswordGenerator r => Sem r Password
generatePassword =
    send (GeneratePassword :: PasswordGenerator (Sem r) Password)

setPassword
    :: Member UserManagement r
    => Username
    -> Password
    -> Sem r (ExitCode, String, String)
setPassword user password = send (SetPassword user password)

createUser
    :: Member UserManagement r
    => Username
    -> UserGroup
    -> Sem r (ExitCode, String, String)
createUser user group = send (CreateUser user group)

deleteUser :: Member UserManagement r => Username -> Sem r ()
deleteUser user = send (DeleteUser user)

-- | Uses pwgen utility to generate a random password.
newRandomPassword :: (MonadIO m, Members '[Embed m] r) => Sem r Password
newRandomPassword = return "secret"

createUserCmd
    :: (Member (Embed IO) r)
    => Username
    -> UserGroup
    -> Sem r (ExitCode, String, String)
createUserCmd user group =
    embed $ readCreateProcessWithExitCode (buildCreateUserProc user group) ""

setPasswordCmd
    :: (Member (Embed IO) r)
    => Username
    -> Password
    -> Sem r (ExitCode, String, String)
setPasswordCmd user Password { password } =
    embed $ readCreateProcessWithExitCode
        (buildSetPasswordProc user)
        (T.unpack $ T.unlines [password, password])

deleteUserCmd :: (Member (Embed IO) r) => Username -> Sem r ()
deleteUserCmd user = do
    _ <- embed $ readCreateProcessWithExitCode (buildDeleteUserProc user) ""
    return ()

buildCreateUserProc :: Username -> UserGroup -> CreateProcess
buildCreateUserProc Username {..} UserGroup {..} =
    proc "useradd"
        $  ["-m", T.unpack username, "-g", T.unpack group]
        ++ maybe [] (\p -> ["-b", T.unpack p]) prefix

buildSetPasswordProc :: Username -> CreateProcess
buildSetPasswordProc Username { username } = proc "passwd" [T.unpack username]

buildDeleteUserProc :: Username -> CreateProcess
buildDeleteUserProc Username { username } =
    proc "userdel" ["-r", T.unpack username]
