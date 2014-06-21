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
                               }

type Position = Int

data Rule = Rule { dateField :: Position
                   , descField :: [Position]
                   , textField :: [Position]
                   , balanceField :: Position
                 }

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



