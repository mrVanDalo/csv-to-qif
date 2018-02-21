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
import Test.QuickCheck
import Data.Char(isPrint)

import Qifer
import QifData
import QifParser


main :: IO ()
main = hspec $
  describe "Csv" $ do
  it "should transform CSV" $ do
      let rule = Rule 1 [2] [0,3] 4
          trans= Transaction "2" "3" "1 4" "5"
          csv  = [["1","2","3","4","5"]]
      toTransactions rule csv `shouldBe` [trans]
  it "should generate Qif" $ do
      let trans = Transaction "heute" "short" "long bla bla" "100$"
          _qif  = ["Pshort","T100$","Dheute","Mlong bla bla"]
      toQif trans `shouldBe` _qif
  it "should generate Qif File content" $ do
      let trans = Transaction "heute" "short" "long bla bla" "100$"
          _qif  = ["!Type:Bank","Pshort","T100$","Dheute","Mlong bla bla","^"]
      transToQif [trans] `shouldBe` _qif
  it "should update a transaction" $ do
      let trans  = Transaction "heute" "short" "long bla bla" "100$"
          theUpdate = Transaction "heute" "bam" "long bla bla" "100$"
      updateTransaction "ort" "bam" trans `shouldBe` theUpdate
      updateTransaction "foo" "bam" trans `shouldBe` trans
  it "should update multiple transactions" $ do
      let transA       = Transaction "heute"  "short story long its .." "long bla bla" "100$"
          transB       = Transaction "morgen" "balla ball bam bam boom"   "long bla bla" "100$"
          transA1      = Transaction "heute"  "found story" "long bla bla" "100$"
          transB1      = Transaction "morgen" "found balla" "long bla bla" "100$"
          regex        = [("story","found story"),("balla","found balla")]
          transactionsToTest = [transA,transB]
      update regex transactionsToTest `shouldBe` [transA1,transB1]
  it "taking a qif, printing it as a string, and parsing it should result in the original qif" $
    property $ forAll qif $ \qif -> qifFromString (qifToString qif) == qif



transaction :: Gen Transaction
transaction = do
   i <- choose(0,20)
   j <- choose(0,20)
   k <- choose(0,20)
   m <- choose(0,20)
   transactionDate        <- suchThat (vector i) isText
   transactionDescription <- suchThat (vector j) isText
   transactionText        <- suchThat (vector k) isText
   transactionBalance     <- suchThat (vector m) isText
   return Transaction{ date = transactionDate, description = transactionDescription, text = transactionText , balance = transactionBalance }

qif :: Gen Qif
qif = do i <- choose(0,20)
         typeinfoToTest    <- suchThat (vector i) isText
         transactionsToTest <- listOf transaction
         return Qif{ typeinfo = typeinfoToTest, transactions = transactionsToTest }

isText :: String -> Bool
isText = all isPrint
