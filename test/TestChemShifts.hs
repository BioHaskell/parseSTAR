{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main(main)
where

import System.Environment(getArgs, getProgName)
import System.IO(hPrint, hPutStr, hPutStrLn, stderr)
import Data.Binary
import System.Exit(exitFailure, exitSuccess)
import Control.Monad(when)

import Data.STAR.ChemShifts(ChemShift(..), parse)

printUsage = do prog <- getProgName
                hPutStrLn stderr $ usageStr prog
  where
    usageStr prog = concat ["Usage: ", prog, " <input.str> <output.cs>"]

main = do l <- length `fmap` getArgs
          when (l /= 2) $ do printUsage
                             exitFailure
          [input, output] <- getArgs
          dat <- parse input
          case dat of
            Left  err    -> do hPutStr stderr $ "Error parsing " ++ input ++ ": "
                               hPutStrLn stderr err
                               exitFailure
            Right parsed -> do putStrLn $ concat ["Parsed ", show $ length parsed, " chemical shifts."]
                               Data.Binary.encodeFile output (parsed :: [ChemShift])
                               exitSuccess
