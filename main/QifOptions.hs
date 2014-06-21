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

import System.Console.GetOpt
import System.Exit
import System.IO
import System.Environment
import Control.Monad
import Text.Read
import Data.List.Split
import Data.Maybe



data Options = Options  { optVerbose    :: Bool
                        , optInput      :: String
                        , optOutput     :: String
                        , optDate       :: Maybe Int
                        , optBalance    :: Maybe Int
                        , optText       :: [Int]
                        , optLongText   :: [Int]
                        , optSkip       :: Int
                        , optSeparator  :: Char
                        } deriving (Show)

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = ""
                        , optOutput     = ""
                        , optDate       = Nothing
                        , optBalance    = Nothing
                        , optText       = []
                        , optLongText   = []
                        , optSkip       = 0
                        , optSeparator  = ','
                        }

instance Show (a -> b) where
    show _ = "<function>"

instance Show (IO a) where
    show _ = "<IoString>"


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = arg })
            "FILE")
        "Input file (CSV Format)"

    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = arg })
            "FILE")
        "Output file (Qif Format)"

    , Option "z" ["separator"]
        (ReqArg
            (\arg opt -> return opt { optSeparator = head arg })
            "char")
        "separator of the csv file"

    , Option "d" ["date"]
        (ReqArg
            (\arg opt -> return opt { optDate = readColumn arg})
            "<column number>")
        "Date Column"
    , Option "b" ["balance"]
        (ReqArg
            (\arg opt -> return opt { optBalance = readColumn arg})
            "<column number>")
        "Balance Column"
    , Option "t" ["text"]
        (ReqArg
            (\arg opt -> return opt { optText = readColumns arg})
            "<column numbers>")
        "Text Columns"
    , Option "l" ["longtext"]
        (ReqArg
            (\arg opt -> return opt { optLongText = readColumns arg})
            "<column numbers>")
        "Long Text Columns"
    , Option "s" ["skip"]
        (ReqArg
            (\arg opt -> case (readColumn arg) of
                Nothing -> return opt
                Just r  -> return opt { optSkip = r} )
            "<number of rows>")
        "Rows to Skip before reading"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.2"
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo (useage prg) options)
                exitWith ExitSuccess))
        "Show help"
    ]

useage :: String -> String
useage prog = prog ++ "\n\n" ++
    "author : Ingolf Wagner\n\n" ++
    header ++ "\n\n" ++
    information ++ "\n\n"
    where   header      = "Converting CSV files to Qif Files"
            information = "all column numbers start at 0!"


readColumn :: String -> Maybe Int
readColumn input = readMaybe input

readColumns :: String -> [Int]
readColumns input = catMaybes $ map readColumn $ splitOn "," input

