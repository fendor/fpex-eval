module Parser where

import           Test.Tasty.Hspec
import           Fpex.Eval.Types
import           Fpex.Parse.Input
import           Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = describe "parser tests" $ do
  it "Parse first exercise" $ do
    parseResult <- liftIO $ parseTestSpecification' "testdata/ws19_fp_ueb01_testfaelle.tex"
    parseResult `shouldBe` Right testgroups

  it "Parse first exercise" $ do
    parseResult <- liftIO $ parseTestSpecification' "testdata/ws19_fp_ueb02_testfaelle.tex"
    parseResult `shouldBe` Right moretests

moretests :: [TestGroup TestCase]
moretests =
  [ TestGroup { label = ""
              , pointsPerTest = Points { getPoints = 5 }
              , penalty = Points { getPoints = 0 }
              , maximal = Points { getPoints = 19 }
              , group =
                  [ TestCase { query = "dp (1,20)"
                             , expectedOutput = "[2,3,5,7,11,13,17]"
                             }
                  , TestCase { query = "(dp (1,32),[i|i<-[1..32],dp(i,i)==[i]])"
                             , expectedOutput =
                                 "([2,3,5,7,11,13,17,31],[2,3,5,7,11,13,17,31])"
                             }
                  , TestCase { query = "dp (33,99)"
                             , expectedOutput = "[37,71,73,79,97]"
                             }
                  , TestCase { query = "dp (1021,1034)"
                             , expectedOutput = "[1021,1031,1033]"
                             }]
              }
  , TestGroup { label = ""
              , pointsPerTest = Points { getPoints = 5 }
              , penalty = Points { getPoints = 0 }
              , maximal = Points { getPoints = 16 }
              , group =
                  [ TestCase { query = "[(length.folge)n|n<-[1..30]]"
                             , expectedOutput =
                                 "[1,2,2,3,2,1,2,3,4,4,2,7,2,5,5,6,2,4,2,7,3,6,2,5,2,7,3,1,2,15]"
                             }
                  , TestCase { query = "[n|n<-[31..149],n/=138,l<-[last(folge n)],l/=1]"
                             , expectedOutput = "[95,119,143]"
                             }
                  , TestCase { query = "let tls[]=[[]];tls(e:l)=(e:l):(tls l);in [n|n<-[1..80],(a:b:_)<-(tls.folge)n,(_:bx:_)<-[folge a],b/=bx]"
                             , expectedOutput = "[]"
                             }
                  , TestCase { query = "take 3 (folge 138)"
                             , expectedOutput = "[138,150,222]"
                             }]
              }
  , TestGroup { label = ""
              , pointsPerTest = Points { getPoints = 3 }
              , penalty = Points { getPoints = 0 }
              , maximal = Points { getPoints = 15 }
              , group =
                  [ TestCase { query = "[medianoid l|l<-[[2,5,7,9,11],[5,2,7,11,9],[5,7,2,11,9],[2,5,7,9,11,13],[2,5,7,9,11,13,17]]]"
                             , expectedOutput = "[7,7,7,9,9]"
                             }
                  , TestCase { query =
                                 "[(medianoid.take 25)[2,i ..]|i<-[2..15]]"
                             , expectedOutput =
                                 "[50,14,26,38,50,62,74,86,98,110,122,134,146,158]"
                             }
                  , TestCase { query = "[(medianoid.take 9)[i,j ..]|i<-[1..4],j<-[i+1 ..6]]"
                             , expectedOutput =
                                 "[5,9,13,17,21,6,10,14,18,7,11,15,8,12]"
                             }]
              }]

testgroups :: [TestGroup TestCase]
testgroups =
  [ TestGroup { label = ""
              , pointsPerTest = Points { getPoints = 10 }
              , penalty = Points { getPoints = 0 }
              , maximal = Points { getPoints = 20 }
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
              , maximal = Points { getPoints = 15 }
              , group =
                  [ TestCase { query =
                                 "[n|n<-[0..99],ist_umgekehrt_2er_potenz n]"
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
              , maximal = Points { getPoints = 15 }
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
