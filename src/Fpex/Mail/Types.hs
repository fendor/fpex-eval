module Fpex.Mail.Types where

import qualified Data.Text.Lazy                as LT

data Mail = Mail
    { from :: String
    , to :: String
    , subject :: String
    , content :: LT.Text
    }