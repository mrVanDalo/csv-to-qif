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

balance_map f x = x{balance = f $ balance x}

type Position = Int

data Rule = Rule { dateField :: Position
                   , descField :: [Position]
                   , textField :: [Position]
                   , balanceField :: Position
                 } deriving (Show)

highestPosition :: Rule -> Position
highestPosition (Rule dF tF ltF bF) =
    maximum . concat $ [[dF],tF,ltF,[bF]]

toTransactions :: Rule -> CSV -> [Transaction]
toTransactions _ []        = []
toTransactions rule (c:sv)
    | (highestPosition rule) >= (length c) = stepDeeper
    | otherwise                            = transform : stepDeeper
    where pick n     = (c !! (n rule))
          pock n     = concat . intersperse " " . map (\k -> c !! k ) $ (n rule)
          stepDeeper = toTransactions rule sv
          transform  = (Transaction (pick dateField)
                                    (pock descField)
                                    (pock textField)
                                    (pick balanceField))

qifHeader :: String -> String
qifHeader typeinfo = "!Type:" ++ typeinfo

toQif :: Transaction -> [String]
toQif trans = ["P" ++ d, "T" ++ money, "D" ++ time, "M" ++ msg]
    where   d     = (description trans)
            money = (balance trans)
            time  = (date trans)
            msg   = (text trans)

transToQif :: String -> [Transaction] -> [String]
transToQif typeinfo trans = (qifHeader typeinfo) : (foo trans)
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
        updateSingle [] r                                 = r
        updateSingle ((rgx,replacement):rest) transaction =
            if updatedTrans == transaction
            then
                updateSingle rest transaction
            else
                updatedTrans
            where
                updatedTrans = updateTransaction rgx replacement transaction
