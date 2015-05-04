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

import QifData
import Text.ParserCombinators.Parsec

newline_skip = do _ <- newline
                  return ()
newline_or_eof = choice [newline_skip, eof]

data TransactionField = D String | P String | M String | T String

date_parser :: GenParser Char st TransactionField
date_parser = do _ <- string "D"
                 date <- manyTill (noneOf ['\n','\r']) (newline_or_eof)
                 return $ D date


description_parser :: GenParser Char st TransactionField
description_parser = do _ <- string "P"
                        description <- manyTill (noneOf ['\n','\r']) newline_or_eof
                        return $ P description

text_parser :: GenParser Char st TransactionField
text_parser = do _ <- string "M"
                 text <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 return $ M text

balance_parser :: GenParser Char st TransactionField
balance_parser = do _ <- string "T"
                    balance <- manyTill (noneOf ['\n','\r']) newline_or_eof
                    return $ T balance


type_parser :: GenParser Char st String
type_parser = do _ <- string "!Type:"
                 typeinfo <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 return typeinfo

transaction_parser :: GenParser Char st Transaction
transaction_parser = do fields <- manyTill (choice [date_parser, description_parser, text_parser, balance_parser]) (try $ lookAhead $ seperator_parser)
                        return $ foldl fieldToTransaction 
                                       Transaction { date = "", description = "", text = "", balance = "" }
                                       fields
              where fieldToTransaction trans (D date) = trans{ date = date }
                    fieldToTransaction trans (P desc) = trans{ description = desc }
                    fieldToTransaction trans (M text) = trans{ text = text }
                    fieldToTransaction trans (T bal)  = trans{ balance = bal }

seperator_parser :: GenParser Char st ()
seperator_parser = do  _ <- string "^" 
                       _ <- newline_or_eof
                       return ()


transactions_parser :: GenParser Char st [Transaction]
transactions_parser = sepEndBy transaction_parser seperator_parser

qif_file_parser :: GenParser Char st Qif
qif_file_parser = do typeinfo     <- type_parser
                     transactions <- transactions_parser
                     return Qif{typeinfo = typeinfo, transactions = transactions }

test_parser p s = parse p "(unknown)" s



parse_qif_file filename = do contents <- readFile filename
                             return $ fromRight (Qif{typeinfo = "Bank", transactions = []}) $ parse qif_file_parser "(unknown)" contents

fromRight d (Right x) = x
fromRight d _         = d



