-----------------------------------------------------------------------------
--
-- Module      :  QifParser
-- Copyright   :  (c) Jonathan Kochems
-- License     :  BSD3
--
-- Maintainer  :  Jonathan Kochems <jonathan.kochems@google.com>
-- Stability   :  unstable
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module QifParser where

import Qifer 
import Text.ParserCombinators.Parsec

newline_skip = do _ <- newline
                  return ()
newline_or_eof = choice [newline_skip, eof]


date_parser :: GenParser Char st String
date_parser = do _ <- string "D"
                 date <- manyTill (noneOf ['\n','\r']) (newline_or_eof)
                 return date


description_parser :: GenParser Char st String
description_parser = do _ <- string "P"
                        description <- manyTill (noneOf ['\n','\r']) newline_or_eof
                        return description

text_parser :: GenParser Char st String
text_parser = do _ <- string "M"
                 text <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 return text

balance_parser :: GenParser Char st String
balance_parser = do _ <- string "T"
                    balance <- manyTill (noneOf ['\n','\r']) newline_or_eof
                    return balance


type_parser :: GenParser Char st String
type_parser = do _ <- string "!Type:"
                 typeinfo <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 return typeinfo

transaction_parser :: GenParser Char st Transaction
transaction_parser = do date <- date_parser
                        description <- description_parser
                        text <- option "" text_parser
                        balance <- balance_parser
                        return $ Transaction { date = date,
                                               description = description,
                                               text = text,
                                               balance = balance
                                             }

transactions_parser :: GenParser Char st [Transaction]
transactions_parser = sepEndBy transaction_parser seperator
    where seperator = do  _ <- string "^" 
                          _ <- newline_or_eof
                          return ()

qif_file_parser :: GenParser Char st (String, [Transaction])
qif_file_parser = do typeinfo     <- type_parser
                     transactions <- transactions_parser
                     return (typeinfo, transactions)

parse_qif_file filename = do contents <- readFile filename
                             return $ fromRight (qifHeader "Bank",[]) $ parse qif_file_parser "(unknown)" contents

fromRight d (Right x) = x
fromRight d _         = d

test_parser p s = parse p "(unknown)" s



