module Fpex.Mail.Simple where

import           Network.Socket                 ( PortNumber )

import           Fpex.Mail.Types                ( Mail(..) )
import           Fpex.Mail.Effect
import           Fpex.User.Types
import           Polysemy
import           Polysemy.Resource
import           Control.Monad                  ( forM_ )

hostname :: String
hostname = "mail.student.tuwien.ac.at"

port :: PortNumber
port = 587

sendMails
    :: Members '[Resource, SMTPConnection, SendMail] r
    => Username
    -> Password
    -> [Mail]
    -> Sem r ()
sendMails username password mails = withConnection
    hostname
    port
    username
    password
    (\conn -> forM_ mails $ \mail -> sendMail mail conn)
