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

import Data.Char
import Data.Spreadsheet
import Control.Monad.Exception.Asynchronous.Lazy(Exceptional(..))
import Data.List
import System.IO.Error
import System.IO

import Qifer




type ParseError = String

-- | parse csv file to read transactions from it
parseCSVFromFile :: Int -> String -> Char -> IO (Either ParseError CSV)
parseCSVFromFile skip file separator = do
    firstContent <- readFile file
    let content = concat . intersperse "\n" . drop skip . lines $ firstContent
    case (fromString '\n' separator content) of
        Exceptional (Just s) result -> do
            hPutStrLn stderr s
            return $ Right $ strip result
        Exceptional Nothing result -> return $ Right $ strip result

strip :: CSV -> CSV
strip = map (\line -> map (\word -> stripWord word) line)
    where   stripWord     = dropWhile dontNeed . dropWhileEnd dontNeed
            dontNeed c    = (c == '"') || (isSpace c)
