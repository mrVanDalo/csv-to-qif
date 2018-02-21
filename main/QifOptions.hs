-----------------------------------------------------------------------------
--
-- Module      :  QifOptions
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
module QifOptions where

import           Data.List.Split       (splitOn)
import           Data.Maybe            (mapMaybe)
import           System.Console.GetOpt (ArgDescr (..), OptDescr (..), usageInfo)
import           System.Environment    (getProgName)
import           System.Exit           (exitSuccess)
import           System.IO             (hPutStrLn, stderr)
import           Text.Read             (readMaybe)

data Options = Options
  { optVerbose   :: Bool
  , optInput     :: String
  , optOutput    :: String
  , optDate      :: Maybe Int
  , optBalance   :: Maybe Int
  , optText      :: [Int]
  , optLongText  :: [Int]
  , optSkip      :: Int
  , optSeparator :: Char
  , optUpdater   :: Maybe String
  } deriving (Show)

startOptions :: Options
startOptions =
  Options
  { optVerbose = False
  , optInput = ""
  , optOutput = ""
  , optDate = Nothing
  , optBalance = Nothing
  , optText = []
  , optLongText = []
  , optSkip = 0
  , optSeparator = ','
  , optUpdater = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option
      "i"
      ["input"]
      (ReqArg (\arg opt -> return opt {optInput = arg}) "FILE")
      "Input file (CSV Format)"
  , Option
      "o"
      ["output"]
      (ReqArg (\arg opt -> return opt {optOutput = arg}) "FILE")
      "Output file (Qif Format)"
  , Option
      "u"
      ["updater"]
      (ReqArg (\arg opt -> return opt {optUpdater = Just arg}) "FILE")
      "Updater file format is `match`<->`replacement`"
  , Option
      "z"
      ["separator"]
      (ReqArg (\arg opt -> return opt {optSeparator = head arg}) "char")
      "separator of the csv file"
  , Option
      "d"
      ["date"]
      (ReqArg
         (\arg opt -> return opt {optDate = readColumn arg})
         "<column number>")
      "Date Column"
  , Option
      "b"
      ["balance"]
      (ReqArg
         (\arg opt -> return opt {optBalance = readColumn arg})
         "<column number>")
      "Balance Column"
  , Option
      "t"
      ["text"]
      (ReqArg
         (\arg opt -> return opt {optText = readColumns arg})
         "<column numbers>")
      "Text Columns"
  , Option
      "l"
      ["longtext"]
      (ReqArg
         (\arg opt -> return opt {optLongText = readColumns arg})
         "<column numbers>")
      "Long Text Columns"
  , Option
      "s"
      ["skip"]
      (ReqArg
         (\arg opt ->
            case readColumn arg of
              Nothing -> return opt
              Just r  -> return opt {optSkip = r})
         "<number of rows>")
      "Rows to Skip before reading"
  , Option
      "v"
      ["verbose"]
      (NoArg (\opt -> return opt {optVerbose = True}))
      "Enable verbose messages"
  , Option
      "V"
      ["version"]
      (NoArg
         (\_ -> do
            hPutStrLn stderr "Version 0.2"
            exitSuccess))
      "Print version"
  , Option
      "h"
      ["help"]
      (NoArg
         (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo (useage prg) options)
            exitSuccess))
      "Show help"
  ]

useage :: String -> String
useage prog =
  prog ++
  "\n\n" ++
  "author : Ingolf Wagner\n\n" ++ header ++ "\n\n" ++ information ++ "\n\n"
  where
    header = "Converting CSV files to Qif Files"
    information = "all column numbers start at 0!"

readColumn :: String -> Maybe Int
readColumn = readMaybe

readColumns :: String -> [Int]
readColumns input = mapMaybe readColumn (splitOn "," input)
