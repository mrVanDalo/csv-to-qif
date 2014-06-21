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
                        , optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , optDate       :: Maybe Int
                        , optBalance    :: Maybe Int
                        , optText       :: [Int]
                        , optLongText   :: [Int]
                        } deriving (Show)

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = getContents
                        , optOutput     = putStr
                        , optDate       = Nothing
                        , optBalance    = Nothing
                        , optText       = []
                        , optLongText   = []
                        }

instance Show (a -> b) where
    show _ = "<function>"

instance Show (IO a) where
    show _ = "<IoString>"


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = readFile arg })
            "FILE")
        "Input file CSV"

    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file Qif"

    , Option "d" ["date"]
        (ReqArg
            (\arg opt -> return opt { optDate = readColumn arg})
            "<column number>")
        "Number of Date Column"
    , Option "b" ["balance"]
        (ReqArg
            (\arg opt -> return opt { optBalance = readColumn arg})
            "<column number>")
        "Number of Balance Column"
    , Option "t" ["text"]
        (ReqArg
            (\arg opt -> return opt { optText = readColumns arg})
            "<column numbers>")
        "Number of Text Columns"
    , Option "l" ["ltext"]
        (ReqArg
            (\arg opt -> return opt { optLongText = readColumns arg})
            "<column numbers>")
        "Number of long Text Columns"
    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

readColumn :: String -> Maybe Int
readColumn input = readMaybe input

readColumns :: String -> [Int]
readColumns input = catMaybes $ map readColumn $ splitOn "," input

