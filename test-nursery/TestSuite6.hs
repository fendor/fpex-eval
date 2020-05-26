{-# LANGUAGE TemplateHaskell #-}

import Assignment6 hiding (main)
import qualified TestSpec as T

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [
      ]



