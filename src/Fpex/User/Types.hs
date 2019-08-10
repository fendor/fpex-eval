module Fpex.User.Types where

import           GHC.Generics                   ( Generic )
import           Data.String                    ( IsString )
import qualified Data.Text                     as T

newtype Username = Username { username :: T.Text }
    deriving (Show, Generic, Eq)
    deriving newtype (IsString)

newtype Password = Password { password :: T.Text }
    deriving (Show, Generic, Eq)
    deriving newtype (IsString)

data UserGroup = UserGroup
    { group :: T.Text
    , prefix :: Maybe T.Text
    }
    deriving (Show, Generic, Eq)

data UserError
    = CreationFailed
    | PasswordNotSet
    deriving (Show, Eq, Ord)
