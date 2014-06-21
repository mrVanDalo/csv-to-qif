-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (main) where

import QifOptions
import Qifer
import Parser

import System.Environment
import System.Console.GetOpt
import System.IO
import Data.Maybe
import System.Exit





-- | the famous main method
main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optInput    = input
                , optOutput   = output   } = opts

    checkArguments opts

    let rules = rule opts
    parseResult <- parseCSVFromFile (optInput opts) (optSeparator opts)
    case parseResult of
        Left error -> do
            hPutStrLn stderr $ error
            exitFailure
        Right csv  -> do
            putStrLn $ show csv
            let toTransform = drop (optSkip opts) csv
                actions = toTransactions rules toTransform
                qif = transToQif actions
            withFile (optOutput opts) WriteMode (\h ->
                mapM_ ( hPutStrLn h) qif)


    putStrLn $ show rules
    putStrLn $ show opts

-- | this function is unsafe call it after checkArguments
-- @todo : make me safer
rule :: Options -> Rule
rule opts = Rule {
    dateField = fromJust $ optDate opts
    , balanceField = fromJust $ optBalance opts
    , textField = optLongText opts
    , descField = optText opts
    }

-- | checks input arguments for minimum of configuration
checkArguments :: Options -> IO ()
checkArguments opts = do
    case errors of
        [] ->
            return ()
        _  -> do
            mapM_ putStrLn  errors
            exitFailure
            return ()
    where
        errors = catMaybes list
        list =  [ (check optDate "need date column")
                , (check optBalance "need balance column")
                , (checkL optText "need text columns")
                , (checkL optLongText "need long text columns")
                , (checkL optInput "need input file")
                ]
        check getter text = case (getter opts) of
                                Nothing -> Just text
                                Just _  -> Nothing
        checkL getter text = case (getter opts) of
                                []      -> Just text
                                _       -> Nothing




