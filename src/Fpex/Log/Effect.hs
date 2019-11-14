module Fpex.Log.Effect where

import           Polysemy
import           Polysemy.Internal
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Fpex.Eval.Types
import           Fpex.Course.Types
import           System.IO                      ( stderr )

data Log m a where
    Debug ::T.Text -> Log m ()
    TraceTestCase ::TestCaseResult -> Log m ()
    LogStudent ::Student -> Log m ()


debug :: Member Log r => T.Text -> Sem r ()
debug t = send (Debug t)

traceTestCase :: Member Log r => TestCaseResult -> Sem r ()
traceTestCase t = send (TraceTestCase t)

logStudent :: Member Log r => Student -> Sem r ()
logStudent t = send (LogStudent t)

runLog :: Member (Embed IO) r => Sem (Log : r) a -> Sem r a
runLog = interpret $ \case
    Debug         t -> embed $ T.hPutStrLn stderr t
    TraceTestCase t -> embed $ T.hPutStrLn stderr $ "\t" <> case t of
        TestCaseRun r        -> "Result: " <> actualOutput r
        TestRunTimeException -> "Run-Time Exception"
        TestCaseCompilefail  -> "Compilation Failed"
        TestCaseNotSubmitted -> "Testcase has not been submitted"
        TestCaseTimeout      -> "Testcase received a timeout"
    LogStudent t ->
        embed $ T.hPutStrLn stderr $ "Evaluate Student: " <> matrNr t

runVoidLog :: Member (Embed IO) r => Sem (Log : r) a -> Sem r a
runVoidLog = interpret $ \case
    Debug         _ -> embed (return () :: IO ())
    TraceTestCase _ -> embed (return () :: IO ())
    LogStudent    _ -> embed (return () :: IO ())
