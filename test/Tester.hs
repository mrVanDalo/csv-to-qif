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
            trans= (Transaction "2" "3" "14" "5")
            csv  = [["1","2","3","4","5"]]
        (toTransactions rule csv) `shouldBe` [trans]
    it "should generate Qif" $ do
        let trans = (Transaction "heute" "short" "long bla bla" "100$")
            qif   = ["Pshort","T100$","Dheute","Mlong bla bla"]
        (toQif trans) `shouldBe` qif
