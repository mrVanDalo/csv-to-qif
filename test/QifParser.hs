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
--
--
-----------------------------------------------------------------------------
module QifParser where

import           QifData
import           Text.ParserCombinators.Parsec

{- **************************************************
    Parsers for transaction fields
*************************************************** -}
-- | Helper type to tag transaction field data
data TransactionField
  = D String
  | P String
  | M String
  | T String

-- | Parser for date field in transaction
dataParser :: GenParser Char st TransactionField
dataParser = do
  _ <- string "D"
  date <- manyTill (noneOf ['\n', '\r']) newLineOrEndofFile
  return $ D date

-- | Parser for description field in transaction
descriptionParser :: GenParser Char st TransactionField
descriptionParser = do
  _ <- string "P"
  description <- manyTill (noneOf ['\n', '\r']) newLineOrEndofFile
  return $ P description

-- | Parser for text field in transaction
textParser :: GenParser Char st TransactionField
textParser = do
  _ <- string "M"
  text <- manyTill (noneOf ['\n', '\r']) newLineOrEndofFile
  return $ M text

-- | Parser for balance field in transaction
balanceParser :: GenParser Char st TransactionField
balanceParser = do
  _ <- string "T"
  balance <- manyTill (noneOf ['\n', '\r']) newLineOrEndofFile
  return $ T balance

{- **************************************************
    transactions
*************************************************** -}
-- | Parser for one whole transaction
transactionParser :: GenParser Char st Transaction
transactionParser =
  foldl
    fieldToTransaction
    Transaction {date = "", description = "", text = "", balance = ""} <$>
  manyTill
    (choice [dataParser, descriptionParser, textParser, balanceParser])
    (try $ lookAhead seperatorParser)

  where
    fieldToTransaction trans (D date) = trans {date = date}
    fieldToTransaction trans (P desc) = trans {description = desc}
    fieldToTransaction trans (M text) = trans {text = text}
    fieldToTransaction trans (T bal)  = trans {balance = bal}

-- | Parser for transaction seperator '^'
seperatorParser :: GenParser Char st ()
seperatorParser = do
  _ <- string "^"
  _ <- newLineOrEndofFile
  return ()

-- | Parser for a list of transactions
transactionsParser :: GenParser Char st [Transaction]
transactionsParser = sepEndBy transactionParser seperatorParser

{- **************************************************
    Qif file
*************************************************** -}
-- | Parser for type of qif (Bank, CCard, etc.)
typeParser :: GenParser Char st String
typeParser = do
  _ <- string "!Type:"
  manyTill (noneOf ['\n', '\r']) newLineOrEndofFile

-- | Parser for a qif file
qifFileParser :: GenParser Char st Qif
qifFileParser = do
  typeinfo <- typeParser
  transactions <- transactionsParser
  return Qif {typeinfo = typeinfo, transactions = transactions}

-- | obtain qif from string
qifFromString string =
  fromRight Qif{typeinfo = "Bank", transactions = []} $
    parse qifFileParser "(unknown)" string

{- **************************************************
    Helper functions
*************************************************** -}
-- | test out parser on string
-- test_parser p s = parse p "(unknown)" s
-- | parses newline but throws away '\n'
newlineSkip = do
  _ <- newline
  return ()

-- | skips newline but also succeeds at end-of-file
newLineOrEndofFile = choice [newlineSkip, eof]

-- | quick and dirty Either exception handling with default value
fromRight d (Right x) = x
fromRight d _         = d
