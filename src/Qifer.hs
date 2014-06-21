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

import Data.List
import Text.Regex.TDFA

type CSV = [Row]
type Row = [Column]
type Column = String

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
          pock n = concat . intersperse " " . map (\k -> c !! k ) $ (n rule)

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

-- | updates a Transaction if regex works
updateTransaction :: String -> String -> Transaction -> Transaction
updateTransaction regex replacement transaction =
    transaction { description = updateDesc }
    where
        updateDesc  = if matches then replacement else (description transaction)
        matches     = (description transaction) =~ regex :: Bool

-- | very inefficient but better than nothing
update :: [(String,String)] -> [Transaction] -> [Transaction]
update _ []         = []
update regex (t:ts) = (updateSingle regex t) : (update regex ts)
    where
        updateSingle [] r = r
        updateSingle ((rgx,replacement):rest) transaction =
            if updatedTrans == transaction
            then
                updateSingle rest transaction
            else
                updatedTrans
            where
                updatedTrans = updateTransaction rgx replacement transaction
