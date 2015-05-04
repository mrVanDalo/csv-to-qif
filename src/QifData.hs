-----------------------------------------------------------------------------
--
-- Module      :  QifData
-- Copyright   :  (c) Ingolf Wagner, (c) Jonathan Kochems
-- License     :  BSD3
--
-- Maintainer  :  Jonathan Kochems <jonathan.kochems@gmail.com>
-- Stability   :  unstable
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------


module QifData where

data Qif = Qif{ typeinfo :: String, transactions :: [Transaction]}
    deriving Eq

typeinfoToString :: String -> String
typeinfoToString typeinfo = "!Type:" ++ typeinfo


qifToLines :: Qif -> [String]
qifToLines qif = (typeinfoToString tinfo) : (foo trans)
    where tinfo      = typeinfo qif 
          trans      = transactions qif
          foo []     = []
          foo (t:ts) = (toQif t) ++ ["^"] ++ (foo ts)

qifToString = unlines . qifToLines 

data Transaction = Transaction { date :: String
                                , description :: String
                                , text :: String
                                , balance :: String
                               } deriving (Eq,Show)

map_on_balance f x = x{balance = f $ balance x}


toQif :: Transaction -> [String]
toQif trans = ["P" ++ d, "T" ++ money, "D" ++ time, "M" ++ msg]
    where   d     = (description trans)
            money = (balance trans)
            time  = (date trans)
            msg   = (text trans)

