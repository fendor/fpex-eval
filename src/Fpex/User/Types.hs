module Fpex.User.Types where

import           GHC.Generics                   ( Generic )
import           Data.String                    ( IsString )
import qualified Data.Text                     as T

newtype Username = Username { getUsername :: T.Text }
    deriving (Show, Generic, Eq)
    deriving newtype (IsString)

newtype Password = Password { getPassword :: T.Text }
    deriving (Show, Generic, Eq)
    deriving newtype (IsString)

data Group = Group
    { getGroup :: T.Text
    , getPrefix :: Maybe T.Text
    }
    deriving (Show, Generic, Eq)

data UserError
    = CreationFailed
    | PasswordNotSet
    deriving (Show, Eq, Ord)
