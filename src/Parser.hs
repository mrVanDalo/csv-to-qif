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


import qualified Text.CSV


type CSV = [Row]
type Row = [Column]
type Column = String

type ParseError = String

parseCSVFromFile :: String -> IO (Either ParseError CSV)
parseCSVFromFile file = do
    foo <- Text.CSV.parseCSVFromFile file
    case foo of
        Right csv  -> return $ Right csv
        Left error -> return $ Left $ show error



