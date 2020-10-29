import Fpex.Main (defaultMain)
import Main.Utf8

main :: IO ()
main = withUtf8 defaultMain
