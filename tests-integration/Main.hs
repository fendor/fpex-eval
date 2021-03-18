{-# LANGUAGE CPP #-}
module Main where

import Control.Monad
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.Function
import Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Fpex.Course.Types
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec

integrationRoot :: FilePath
integrationRoot = "." </> ".integration"

testdataRoot :: FilePath
testdataRoot = "." </> "testdata"

environmentRoot :: FilePath
environmentRoot = "." </> "testdata" </> "environment"

main :: IO ()
main = do
  createDirectoryIfMissing True integrationRoot
  ghcEnv <- generateEnvironmentFile
  hspec (tests ghcEnv)

tests :: Text.Text -> Spec
tests ghcEnv = describe "Spec Integration Tests" $ do
  around_
    ( \action -> do
        _ <- setupTestEnvironment ghcEnv action
        pure ()
    )
    $ do
      setupTests
      collectTests
      gradeTests
      feedbackTests

setupTests :: Spec
setupTests =
  describe
    "Setup Tests"
    $ do
      it "Creates \"course.json\"" $ do
        fpex_ ["setup", "--course-dir", ".." </> ".." </> "testdata" </> "course1-state1", "--user-regex", "[0-9]{7}"]
        exists <- doesFileExist "course.json"
        unless exists $ fail "\"course.json\" has not been created"
        Aeson.eitherDecodeFileStrict "course.json"
          >>= \case
            Left err -> fail $ "Failed to decode \"course.json\": " ++ err
            Right (_ :: Course) -> pure ()
      it "Discovers all students" $ do
        fpex_ ["setup", "--course-dir", ".." </> ".." </> "testdata" </> "course1-state1", "--user-regex", "[0-9]{7}"]
        Aeson.eitherDecodeFileStrict "course.json"
          >>= \case
            Left err -> fail $ "Failed to decode \"course.json\": " ++ err
            Right c -> do
              length (courseParticipants c) `shouldBe` 7

collectTests :: Spec
collectTests = do
  before (fpex_ ["setup", "--course-dir", ".." </> ".." </> "testdata" </> "course1-state1", "--user-regex", "[0-9]{7}"]) $
    describe "Collect Tests" $ do
      it "Collect testdata, single run" $ do
        fpex_ ["collect", "--name", "Assignment1", "-n", "1"]
        doesDirectoryExist "Assignment1-1" `shouldReturn` True
        -- 7 students + 1 error Student
        fmap length (listDirectory "Assignment1-1") `shouldReturn` 8

      it "Collect testdata, two run" $ do
        fpex_ ["collect", "--name", "Assignment1", "-n", "1"]
        fpex_ ["collect", "--name", "Assignment1", "-n", "2"]
        doesDirectoryExist "Assignment1-1" `shouldReturn` True
        doesDirectoryExist "Assignment1-2" `shouldReturn` True
        -- 7 students + 1 error Student
        fmap length (listDirectory "Assignment1-1") `shouldReturn` 8
        fmap length (listDirectory "Assignment1-2") `shouldReturn` 8

gradeTests :: Spec
gradeTests = do
  before
    ( do
        fpex_ ["setup", "--course-dir", ".." </> ".." </> "testdata" </> "course1-state1", "--user-regex", "[0-9]{7}"]
        fpex_ ["collect", "--name", "Assignment1", "-n", "1"]
    )
    $ do
      describe "Grade tests" $ do
        it "Grade all students" $ do
          fpex_
            [ "grade",
              "--name",
              "Assignment1",
              "-n",
              "1",
              "--definitions",
              "Assignment1.hs",
              "-t",
              "0.5",
              "--test-suite",
              "TestSuite1.hs"
            ]
          entries <- filterM doesDirectoryExist =<< listDirectory "Assignment1-1"
          forM_ entries $ \dir -> do
            let shouldExist = dir /= "errorStudent"
            doesFileExist ("Assignment1-1" </> dir </> "report.json") `shouldReturn` shouldExist

        it "Grade single student" $ do
          fpex_ ["grade", "--name", "Assignment1", "-n", "1", "--definitions", "Assignment1.hs", "-t", "0.5", "--test-suite", "TestSuite1.hs", "--student", "1234567"]
          entries <- filterM doesDirectoryExist =<< listDirectory "Assignment1-1"
          forM_ entries $ \dir -> do
            doesFileExist ("Assignment1-1" </> dir </> "report.json") `shouldReturn` bool False True (dir == "1234567")

feedbackTests :: Spec
feedbackTests = do
  before
    ( do
        fpex_ ["setup", "--course-dir", ".." </> ".." </> "testdata" </> "course1-state1", "--user-regex", "[0-9]{7}"]
        fpex_ ["collect", "--name", "Assignment1", "-n", "1"]
        fpex_ ["grade", "--name", "Assignment1", "-n", "1", "--definitions", "Assignment1.hs", "-t", "0.5", "--test-suite", "TestSuite1.hs"]
    )
    $ do
      describe "Feedback tests" $ do
        it "Generate feedback for all students" $ do
          fpex_
            [ "feedback",
              "--name",
              "Assignment1",
              "-n",
              "1"
            ]
          entries <- filterM doesDirectoryExist =<< listDirectory "Assignment1-1"
          forM_ entries $ \dir -> do
            let shouldExist = dir /= "errorStudent"
            doesFileExist ("Assignment1-1" </> dir </> "Assignment1.hs.out_1") `shouldReturn` shouldExist
            doesFileExist ("Assignment1-1" </> dir </> "Assignment1_TestSuite1.hs") `shouldReturn` shouldExist

        it "Generate feedback for single student" $ do
          fpex_
            [ "feedback",
              "--name",
              "Assignment1",
              "-n",
              "1",
              "--student",
              "1234567"
            ]
          entries <- listDirectory "Assignment1-1"
          forM_ entries $ \dir -> unless (dir == "Main.hs") $ do
            doesFileExist ("Assignment1-1" </> dir </> "Assignment1.hs.out_1") `shouldReturn` bool False True (dir == "1234567")
            doesFileExist ("Assignment1-1" </> dir </> "Assignment1_TestSuite1.hs") `shouldReturn` bool False True (dir == "1234567")

fpex :: [String] -> IO String
fpex args = do
  (exitCode, sout, serr) <- readCreateProcessWithExitCode fpexProc ""
  case exitCode of
    ExitSuccess -> pure sout
    ExitFailure n -> do
      hPutStrLn stdout $ "Failed to execute fpex command (" ++ show n ++ "): " ++ show fpexProc
      hPutStrLn stdout "-- Stdout ----------------------------------------------------"
      hPutStrLn stdout sout
      hPutStrLn stdout "-- Stderr ----------------------------------------------------"
      hPutStrLn stdout serr
      hPutStrLn stdout "--------------------------------------------------------------"
      fail "Could not execute 'fpex' command."
  where
    fpexProc =
      proc
        "fpex"
        args

fpex_ :: [String] -> IO ()
fpex_ = void . fpex

setupTestEnvironment :: Text.Text -> IO a -> IO a
setupTestEnvironment ghcEnvFile action =
  withTempDirectory integrationRoot "integration" $ \dir -> do
    copyFile (testdataRoot </> "Assignment1.hs") (dir </> "Assignment1.hs")
    copyFile (testdataRoot </> "TestSuite1.hs") (dir </> "TestSuite1.hs")
    withCurrentDirectory dir $ do
      Text.writeFile ".ghc.environment.fpex" ghcEnvFile
      action

generateEnvironmentFile :: IO Text.Text
generateEnvironmentFile = do
  withCurrentDirectory environmentRoot $ do
    let cabalProc = proc "cabal" ["build"]
    _ <- readCreateProcess cabalProc ""
    files <- listDirectory "."
    case find isGhcEnvironmentFile files of
      Nothing -> fail "Could not generate the ghc environment"
      Just f -> do
        contents <- Text.readFile f
        contents
          & Text.lines
          & filter (\l -> not $ any (`Text.isInfixOf` l) illegalLines)
          & Text.unlines
          & pure
  where
    illegalLines :: [Text.Text]
    illegalLines = ["dist-newstyle", "environment"]

isGhcEnvironmentFile :: FilePath -> Bool
#if __GLASGOW_HASKELL__ >= 810
isGhcEnvironmentFile = (isInfixOf "8.10") . takeFileName
#elif __GLASGOW_HASKELL__ >= 808
isGhcEnvironmentFile = (isInfixOf "8.8") . takeFileName
#elif __GLASGOW_HASKELL__ >= 806
isGhcEnvironmentFile = (isInfixOf "8.6") . takeFileName
#else
isGhcEnvironmentFile = isPrefixOf ".ghc.environment." . takeFileName
#endif