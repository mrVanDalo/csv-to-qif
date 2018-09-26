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
-----------------------------------------------------------------------------
module Main
  ( main
  ) where

import           Data.List.Split
import           Data.Maybe
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Parser
import           Qifer
import           QifOptions

-- | the famous main method
main = do
  args <- getArgs
    -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
    -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return startOptions) actions
  let Options {optInput = input, optOutput = output} = opts
  checkArguments opts
  let rules = rule opts
  parseResult <-
    parseCSVFromFile (optSkip opts) (optInput opts) (optSeparator opts)
  case parseResult of
    Left error -> do
      hPutStrLn stderr error
      exitFailure
    Right csv -> do
      let actions = toTransactions rules csv
      updatedActions <-
        case optUpdater opts of
          Just file -> do
            updaterConfig <- readUpdaterFile file
            return $ update updaterConfig actions
          Nothing -> return actions
      let qif = transToQif updatedActions
      withFile (optOutput opts) WriteMode (\h -> mapM_ (hPutStrLn h) qif)

-- | reads updater file
readUpdaterFile :: String -> IO [(String, String)]
readUpdaterFile fileName = tupler . onlyTuple . splitter . lines <$> readFile fileName
  where
    tupler = map (\[x, y] -> (x, y))
    onlyTuple = filter (\l -> length l == 2)
    splitter = map (splitOn "<->")

-- | this function is unsafe call it after checkArguments
-- @todo : make me safer
rule :: Options -> Rule
rule opts =
  Rule
  { dateField = fromJust $ optDate opts
  , balanceField = fromJust $ optBalance opts
  , textField = optLongText opts
  , descField = optText opts
  }

-- | checks input arguments for minimum of configuration
checkArguments :: Options -> IO ()
checkArguments opts =
  case errors of
    [] -> return ()
    _ -> do
      mapM_ putStrLn errors
      exitFailure
      return ()
  where
    errors = catMaybes list
    list =
      [ check optDate "need date column"
      , check optBalance "need balance column"
      , checkL optText "need text columns"
      , checkL optLongText "need long text columns"
      , checkL optInput "need input file"
      ]
    check getter text =
      case getter opts of
        Nothing -> Just text
        Just _  -> Nothing
    checkL getter text =
      case getter opts of
        [] -> Just text
        _  -> Nothing
