{-# LANGUAGE OverloadedStrings #-}

module Fpex.Parse.Input where

import qualified Fpex.Eval.Types as Fpex
import qualified Language.Haskell.Exts.Parser as E
import qualified Language.Haskell.Exts.ExactPrint as E
import qualified Language.Haskell.Exts.Syntax as E
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Void
import           Data.Coerce (coerce)
import           Data.Maybe (fromMaybe)

type Parser = Parsec Void Text

parseTestSpecification' :: FilePath
          -> IO (Either (ParseErrorBundle Text Void) [Fpex.TestGroup Fpex.TestCase])
parseTestSpecification' fp = do
  contents <- T.readFile fp
  return $ parseTestSpecification (Just fp) contents


parseTestSpecification :: Maybe String
          -> Text
          -> Either (ParseErrorBundle Text Void) [Fpex.TestGroup Fpex.TestCase]
parseTestSpecification fpM input =
  let contents' = T.unlines $ filter (not . T.isPrefixOf "#") (T.lines input)
  in parse parser (fromMaybe "" fpM) contents'

parser :: Parser [Fpex.TestGroup Fpex.TestCase]
parser = do
  space
  label "Test Group Parser" $ many parseSingleTestGroup

parseSingleTestGroup :: Parser (Fpex.TestGroup Fpex.TestCase)
parseSingleTestGroup = do
  -- TODO: sanity check
  space
  (perTest, penalty, _) <- points
  space
  testCases <- label "Parse test groups"
    $ many
    $ do
      tc <- testCaseParser
      space
      return tc
  return
    Fpex.TestGroup { label = ""
                   , pointsPerTest = coerce perTest
                   , penalty = coerce penalty
                   , group = testCases
                   }

eol' :: Parser ()
eol' = do
  _ <- eol
  return ()

points :: Parser (Int, Int, Int)
points = do
  _ <- string "--"
  space
  pointsPerTest <- some digitChar
  skipSome (satisfy (== '/') <|> spaceChar)
  negativePoints <- some digitChar
  skipSome (satisfy (== '/') <|> spaceChar)
  allPoints <- some digitChar
  return (read pointsPerTest, read negativePoints, read allPoints)

testCaseParser :: Parser Fpex.TestCase
testCaseParser = do
  (_, testCaseString) <- testCase
  -- TODO: check
  let Just (exprToTest, expectedOutput) =
        parseTestCaseSpecification testCaseString
  return
    Fpex.TestCase { query = T.strip $ T.pack exprToTest
                  , expectedOutput = T.strip $ T.pack expectedOutput
                  }

testCase :: Parser ([Int], String)
testCase = do
  testCaseNumber <- between
    (string "{-")
    (string "-}")
    (label "Parse test number" $ sepBy1 (some digitChar) (string ","))
  testCaseString <- many printChar
  return (map read testCaseNumber, testCaseString)

parseTestCaseSpecification :: String -> Maybe (String, String)
parseTestCaseSpecification input =
  let parsedAst = E.parse input
  in case parsedAst of
       E.ParseFailed _ _ -> Nothing
       E.ParseOk expr    -> case expr of
         (E.InfixApp _ inputExpr _ expectedExpr)
           -> Just (E.exactPrint inputExpr [], E.exactPrint expectedExpr [])
         _ -> Nothing