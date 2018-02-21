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

{- **************************************************
    Parsers for transaction fields
*************************************************** -}
-- | Helper type to tag transaction field data
data TransactionField = D String | P String | M String | T String


-- | Parser for date field in transaction
date_parser :: GenParser Char st TransactionField
date_parser = do _ <- string "D"
                 date <- manyTill (noneOf ['\n','\r']) (newline_or_eof)
                 return $ D date

-- | Parser for description field in transaction
description_parser :: GenParser Char st TransactionField
description_parser = do _ <- string "P"
                        description <- manyTill (noneOf ['\n','\r']) newline_or_eof
                        return $ P description

-- | Parser for text field in transaction
text_parser :: GenParser Char st TransactionField
text_parser = do _ <- string "M"
                 text <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 return $ M text

-- | Parser for balance field in transaction
balance_parser :: GenParser Char st TransactionField
balance_parser = do _ <- string "T"
                    balance <- manyTill (noneOf ['\n','\r']) newline_or_eof
                    return $ T balance

{- **************************************************
    transactions
*************************************************** -}

-- | Parser for one whole transaction
transaction_parser :: GenParser Char st Transaction
transaction_parser = do fields <- manyTill (choice [date_parser, description_parser, text_parser, balance_parser]) (try $ lookAhead $ seperator_parser)
                        return $ foldl fieldToTransaction 
                                       Transaction { date = "", description = "", text = "", balance = "" }
                                       fields
              where fieldToTransaction trans (D date) = trans{ date = date }
                    fieldToTransaction trans (P desc) = trans{ description = desc }
                    fieldToTransaction trans (M text) = trans{ text = text }
                    fieldToTransaction trans (T bal)  = trans{ balance = bal }

-- | Parser for transaction seperator '^'
seperator_parser :: GenParser Char st ()
seperator_parser = do  _ <- string "^" 
                       _ <- newline_or_eof
                       return ()

-- | Parser for a list of transactions
transactions_parser :: GenParser Char st [Transaction]
transactions_parser = sepEndBy transaction_parser seperator_parser


{- **************************************************
    Qif file
*************************************************** -}
-- | Parser for type of qif (Bank, CCard, etc.)
type_parser :: GenParser Char st String
type_parser = do _ <- string "!Type:"
                 typeinfo <- manyTill (noneOf ['\n','\r']) newline_or_eof
                 return typeinfo

-- | Parser for a qif file
qif_file_parser :: GenParser Char st Qif
qif_file_parser = do typeinfo     <- type_parser
                     transactions <- transactions_parser
                     return Qif{typeinfo = typeinfo, transactions = transactions }

-- | obtain qif from string
qifFromString s = fromRight (Qif{typeinfo = "Bank", transactions = []}) $ parse qif_file_parser "(unknown)" s

-- | IO Monad helper function: reads and parses qif file
-- * if parsing successful returns qif data structure, otherwise returns "empty" qif
parse_qif_file filename = do contents <- readFile filename
                             return $ qifFromString contents


{- **************************************************
    Helper functions
*************************************************** -}
-- | test out parser on string
-- test_parser p s = parse p "(unknown)" s

-- | parses newline but throws away '\n'
newline_skip = do _ <- newline
                  return ()

-- | skips newline but also succeeds at end-of-file
newline_or_eof = choice [newline_skip, eof]

-- | quick and dirty Either exception handling with default value
fromRight d (Right x) = x
fromRight d _         = d



