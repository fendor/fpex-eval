module Fpex.Mail.Effect where


import           Polysemy
import           Polysemy.Internal
import           Polysemy.Resource
import qualified Data.Text                     as T
import qualified Network.HaskellNet.SMTP       as SMTP
import qualified Network.HaskellNet.Auth       as Auth
import qualified Network.HaskellNet.SMTP.SSL   as SSL
import           Network.Socket                 ( PortNumber )
import           Fpex.Mail.Types
import           Fpex.User.Types

data SendMail m a where
    SendMail ::Mail -> SSL.SMTPConnection -> SendMail m ()

data SMTPConnection m a where
    Authenticate ::Username -> Password -> SSL.SMTPConnection -> SMTPConnection m Bool

    OpenConnection ::String -> PortNumber -> SMTPConnection m SSL.SMTPConnection

    CloseConnection ::SSL.SMTPConnection -> SMTPConnection m ()

runSendMail :: Member (Embed IO) r => Sem (SendMail : r) a -> Sem r a
runSendMail = interpret $ \case
    SendMail m conn -> embed (simpleMail conn m)

withConnection
    :: Members '[Resource, SMTPConnection] r
    => String
    -> PortNumber
    -> Username
    -> Password
    -> (SSL.SMTPConnection -> Sem r ())
    -> Sem r ()
withConnection hostname port username password worker = bracket
    (openConnection hostname port)
    closeConnection
    (\conn -> authenticate username password conn >>= \case
        True  -> worker conn
        False -> return () -- TODO: proper error handling
    )

runWithConnection
    :: Member (Embed IO) r => Sem (SMTPConnection : r) a -> Sem r a
runWithConnection = interpret $ \case
    Authenticate Username {..} Password {..} conn -> embed $ SSL.authenticate
        Auth.PLAIN
        (T.unpack username)
        (T.unpack password)
        conn
    OpenConnection hostname port -> connectionOpened hostname port
    CloseConnection conn         -> embed $ SMTP.closeSMTP conn


connectionOpened
    :: (Member (Embed IO) r) => String -> PortNumber -> Sem r SSL.SMTPConnection
connectionOpened hostname port = embed $ SSL.connectSMTPSTARTTLSWithSettings
    hostname
    SSL.defaultSettingsSMTPSTARTTLS { SSL.sslPort = port }


sendMail :: Member SendMail r => Mail -> SSL.SMTPConnection -> Sem r ()
sendMail mail conn = send (SendMail mail conn)

-- withConnection
--     :: Member SMTPConnection r
--     => String
--     -> PortNumber
--     -> Username
--     -> Password
--     -> (SSL.SMTPConnection -> Sem r ())
--     -> Sem r ()
-- withConnection hostname port username password func =
--     send (WithConnection hostname port username password func)

authenticate
    :: Member SMTPConnection r
    => Username
    -> Password
    -> SSL.SMTPConnection
    -> Sem r Bool
authenticate username password conn =
    send (Authenticate username password conn)

openConnection
    :: Member SMTPConnection r
    => String
    -> PortNumber
    -> Sem r SSL.SMTPConnection
openConnection hostname port = send (OpenConnection hostname port)

closeConnection :: Member SMTPConnection r => SSL.SMTPConnection -> Sem r ()
closeConnection conn = send (CloseConnection conn)



simpleMail :: SMTP.SMTPConnection -> Mail -> IO ()
simpleMail conn Mail {..} = SMTP.sendPlainTextMail to from subject content conn
