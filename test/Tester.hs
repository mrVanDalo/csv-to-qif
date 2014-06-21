-----------------------------------------------------------------------------
--
-- Module      :  Tester
-- Copyright   :  (c) Ingolf Wagner
-- License     :  BSD3
--
-- Maintainer  :  Ingolf Wagner <palipalo9@googlemail.com>
-- Stability   :  unstable
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (main) where


import Test.Hspec
import Qifer

main :: IO ()
main = hspec $ do
  describe "Csv" $ do
    it "should transform CSV" $ do
        let rule = (Rule 1 [2] [0,3] 4)
            trans= (Transaction "2" "3" "1 4" "5")
            csv  = [["1","2","3","4","5"]]
        (toTransactions rule csv) `shouldBe` [trans]
    it "should generate Qif" $ do
        let trans = (Transaction "heute" "short" "long bla bla" "100$")
            qif   = ["Pshort","T100$","Dheute","Mlong bla bla"]
        (toQif trans) `shouldBe` qif
    it "should generate Qif File content" $ do
        let trans = (Transaction "heute" "short" "long bla bla" "100$")
            qif   = ["!Type:Bank","Pshort","T100$","Dheute","Mlong bla bla","^"]
        (transToQif [trans]) `shouldBe` qif
    it "should update a transaction" $ do
        let trans  = (Transaction "heute" "short" "long bla bla" "100$")
            theUpdate = (Transaction "heute" "bam" "long bla bla" "100$")
        (updateTransaction "ort" "bam" trans) `shouldBe` theUpdate
        (updateTransaction "foo" "bam" trans) `shouldBe` trans
    it "should update multiple transactions" $ do
        let transA       = (Transaction "heute"  "short story long its .." "long bla bla" "100$")
            transB       = (Transaction "morgen" "balla ball bam bam boom"   "long bla bla" "100$")
            transA1      = (Transaction "heute"  "found story" "long bla bla" "100$")
            transB1      = (Transaction "morgen" "found balla" "long bla bla" "100$")
            regex        = [("story","found story"),("balla","found balla")]
            transactions = [transA,transB]
        (update regex transactions) `shouldBe` [transA1,transB1]


