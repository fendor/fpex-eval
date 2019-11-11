import qualified Fpex.Parse.Input              as Parser
import           System.Environment
import           System.IO
import qualified Data.Text.Lazy.IO             as T
import qualified Data.Text                     as T
import           Fpex.Eval.Types
import qualified Data.Aeson.Text               as Aeson


main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp, ass] -> Parser.parseTestSpecification' fp >>= \case
            Left  err -> hPrint stderr err

            Right s   -> do
                let suite = TestSuite (T.pack ass) s
                T.putStrLn (Aeson.encodeToLazyText suite)
        _ -> hPutStrLn stderr
                       "USAGE: fpex-to-json <filepath> <assignment-name>"
