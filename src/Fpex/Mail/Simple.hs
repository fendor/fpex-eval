module Fpex.Mail.Simple
    ( sendCustomMailToAll
    , simpleMail
    , Mail(..)
    )
where

import qualified Data.Text.Lazy                as LT
import qualified Network.HaskellNet.SMTP       as SMTP
import qualified Network.HaskellNet.Auth       as Auth
import qualified Network.HaskellNet.SMTP.SSL   as SSL
import           Network.Socket                 ( PortNumber )

hostname :: String
hostname = "mail.student.tuwien.ac.at"

port :: PortNumber
port = 587

data Mail = Mail
    { from :: String
    , to :: String
    , subject :: String
    , content :: LT.Text
    }

sendCustomMailToAll :: Auth.UserName -> Auth.Password -> [Mail] -> IO ()
sendCustomMailToAll user password mails =
    SSL.doSMTPSTARTTLSWithSettings
            hostname
            SSL.defaultSettingsSMTPSTARTTLS { SSL.sslPort = port }
        $ \conn -> SMTP.authenticate Auth.PLAIN user password conn >>= \case
              True  -> mapM_ (simpleMail conn) mails
              False -> return ()

simpleMail :: SMTP.SMTPConnection -> Mail -> IO ()
simpleMail conn Mail {..} = SMTP.sendPlainTextMail to from subject content conn
