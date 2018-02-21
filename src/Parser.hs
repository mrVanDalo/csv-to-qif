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

import           Control.Monad.Exception.Asynchronous.Lazy (Exceptional (..))
import           Data.Char                                 (isSpace)
import           Data.List                                 (dropWhileEnd)
import           Data.Spreadsheet                          (fromString)
import           System.IO                                 (hPutStrLn, stderr)

import           Qifer                                     (CSV)

type ParseError = String

-- | parse csv file to read transactions from it
parseCSVFromFile :: Int -> String -> Char -> IO (Either ParseError CSV)
parseCSVFromFile skip file separator = do
  firstContent <- readFile file
  let content = unlines . separatorAtEndFix . drop skip . lines $ firstContent
      separatorAtEndFix = map (++ " ")
  case fromString '\n' separator content of
    Exceptional (Just s) res -> do
      hPutStrLn stderr s
      return $ Right $ strip res
    Exceptional Nothing res -> return $ Right $ strip res

strip :: CSV -> CSV
strip = map (map stripWord)
  where
    stripWord = dropWhile dontNeed . dropWhileEnd dontNeed
    dontNeed c = (c == '"') || isSpace c
