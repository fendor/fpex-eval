module Parser where

import           Test.Tasty.Hspec
import           Fpex.Eval.Types
import           Fpex.Parse.Input
import           Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = describe "parser tests"
  $ it "Parse first exercise"
  $ do
    parseResult <- liftIO
      $ parseTestSpecification' "testdata/ws19_fp_ueb01_testfaelle.tex"
    parseResult `shouldBe` (Right testgroups)

testgroups :: [TestGroup TestCase]
testgroups =
  [ TestGroup { label = ""
              , pointsPerTest = Points { getPoints = 10 }
              , penalty = Points { getPoints = 0 }
              , group =
                  [ TestCase { query = "[streiche s v 'c'|s<-[\"\",\"c\",\"a\"],v<-[-1..2]]"
                             , expectedOutput =
                                 "[\"\",\"\",\"\",\"\",\"c\",\"c\",\"\",\"c\",\"a\",\"a\",\"a\",\"a\"]"
                             }
                  , TestCase { query = "[streiche s v 'c'|s<-[\"aca\",\"acac\"],v<-[-1..2]]"
                             , expectedOutput =
                                 "[\"aca\",\"aca\",\"aa\",\"aca\",\"acac\",\"acac\",\"aa\",\"aca\"]"
                             }]
              }
  , TestGroup { label = ""
              , pointsPerTest = Points { getPoints = 3 }
              , penalty = Points { getPoints = 0 }
              , group =
                  [ TestCase { query = "[n|n<-[0..99],ist_umgekehrt_2er_potenz n]"
                             , expectedOutput = "[1,2,4,8,10,20,23,40,46,61,80]"
                             }
                  , TestCase { query =
                                 "[n|n<-[100..160],ist_umgekehrt_2er_potenz n]"
                             , expectedOutput = "[100]"
                             }
                  , TestCase { query = "[n|i<-[0..29],n<-[(read.reverse.show)(2^i)::Int],not(ist_umgekehrt_2er_potenz n)]"
                             , expectedOutput = "[]"
                             }
                  , TestCase { query = "[n|i<-[0..29],n<-[(read.reverse.show)(1+2^i)::Int],ist_umgekehrt_2er_potenz n]"
                             , expectedOutput = "[2]"
                             }
                  , TestCase { query = "[n|i<-[0..26],n<-[((10*).read.reverse.show)(2^i)::Int],not(ist_umgekehrt_2er_potenz n)]"
                             , expectedOutput = "[]"
                             }]
              }
  , TestGroup { label = ""
              , pointsPerTest = Points { getPoints = 3 }
              , penalty = Points { getPoints = 0 }
              , group =
                  [ TestCase { query =
                                 "[n|n<-[0..200],groesstes_palindrom_in[n]==n]"
                             , expectedOutput =
                                 "[0,1,2,3,4,5,6,7,8,9,11,22,33,44,55,66,77,88,99,101,111,121,131,141,151,161,171,181,191]"
                             }
                  , TestCase { query = "[n|n<-[1000..1200],groesstes_palindrom_in[n]==n]"
                             , expectedOutput = "[1001,1111]"
                             }
                  , TestCase { query = "groesstes_palindrom_in[0..120]"
                             , expectedOutput = "111"
                             }
                  , TestCase { query = "groesstes_palindrom_in[0..500]"
                             , expectedOutput = "494"
                             }
                  , TestCase { query = "groesstes_palindrom_in(reverse[0..500])"
                             , expectedOutput = "494"
                             }]
              }]
