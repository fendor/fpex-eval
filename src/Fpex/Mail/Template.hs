module Fpex.Mail.Template where

import qualified Data.Text.Lazy                as LT
import           Fpex.Mail.Simple               ( Mail(..) )
import           Fpex.User.Types

userCreatedMail :: Password -> Mail -> Mail
userCreatedMail password mail = mail { content = newUserTemplate password }

newUserTemplate :: Password -> LT.Text
newUserTemplate Password { getPassword = password } = LT.unlines
    [ "Sehr geehrte(r) Teilnehmer(in)"
    , ""
    , "Sie sind zur LVA 185.A03 Funktionale Programmierung angemeldet."
    , "Bitte loggen Sie sich auf der g0.complang.tuwien.ac.at unter"
    , "fJJKKNNN ein (JJKKNNN durch Ihre Matrikelnummer ersetzen) mit"
    , LT.fromStrict password
    , "und aendern Sie dieses sogleich!"
    , ""
    , "Weitere Informationen:"
    , ""
    , "http://www.complang.tuwien.ac.at/knoop/fp185A03_ws1516"
    , "und die TISS-News von 185.A03"
    , "Viel Erfolg!"
    , ""
    , "DVR 0005886"
    ]
