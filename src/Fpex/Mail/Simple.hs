module Fpex.Mail.Simple
    ( sendSimpleMail
    , simpleMail
    )
where

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Network.Mail.SMTP             as SMTP
import qualified Network.Mail.Mime             as Mime

hostname = "mail.student.tuwien.ac.at"

type From = SMTP.Address
type To = SMTP.Address
type Subject = T.Text
type Content = T.Text

simpleMail :: From -> To -> Subject -> Content -> Mime.Mail
simpleMail from to subject content = SMTP.simpleMail
    from
    [to]
    []
    []
    subject
    [Mime.plainPart (LT.fromStrict content)]

sendSimpleMail :: From -> To -> Subject -> Content -> IO Mime.Mail
sendSimpleMail from to subject content = do
    let mail = simpleMail from to subject content
    SMTP.sendMail hostname mail
    return mail
