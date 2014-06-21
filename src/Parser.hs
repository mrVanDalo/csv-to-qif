{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :  (c) Ingolf Wagner
-- License     :  BSD3
--
-- Maintainer  :  Ingolf Wagner <palipalo9@googlemail.com>
-- Stability   :  unstable
-- Portability :
--
-- | This package should create a parser for csv files.
-- it's just a wrapper
--
-----------------------------------------------------------------------------

module Parser where

import Data.Spreadsheet
import Control.Monad.Exception.Asynchronous.Lazy(Exceptional(..))


type CSV = [Row]
type Row = [Column]
type Column = String

type ParseError = String

-- | parse csv file to read transactions from it
parseCSVFromFile :: String -> Char -> IO (Either ParseError CSV)
parseCSVFromFile file separator = do
    content <- readFile file
    case (fromString '"' separator content) of
        Exceptional (Just s) _ -> return $ Left s
        Exceptional Nothing result -> return $ Right result
