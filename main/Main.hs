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
import System.Environment
import System.Console.GetOpt




main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optVerbose = verbose
                , optInput = input
                , optOutput = output   } = opts

    putStrLn $ show opts

