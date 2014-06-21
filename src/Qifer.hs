-----------------------------------------------------------------------------
--
-- Module      :  Qifer
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

module Qifer where

import Text.CSV

data Transaction = Transaction { date :: String
                                , description :: String
                                , text :: String
                                , balance :: String
                               } deriving (Eq,Show)

type Position = Int

data Rule = Rule { dateField :: Position
                   , descField :: [Position]
                   , textField :: [Position]
                   , balanceField :: Position
                 } deriving (Show)

toTransactions :: Rule -> CSV -> [Transaction]
toTransactions _ [] = []
toTransactions rule (c:sv) =
    ((Transaction (pick dateField)
                  (pock descField)
                  (pock textField)
                  (pick balanceField)
                 ) : (toTransactions rule sv ))
    where pick n = (c !! (n rule))
          pock n = concat . map (\k -> c !! k ) $ (n rule)

qifHeader :: String
qifHeader = "!Type:Bank"

toQif :: Transaction -> [String]
toQif trans = ["P" ++ d, "T" ++ money, "D" ++ time, "M" ++ msg]
    where   d     = (description trans)
            money = (balance trans)
            time  = (date trans)
            msg   = (text trans)

transToQif :: [Transaction] -> [String]
transToQif trans = qifHeader : (foo trans)
    where   foo []     = []
            foo (t:ts) = (toQif t) ++ ["^"] ++ (foo ts)

