{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module Main(main)
where

import System.Environment(getArgs, getProgName)
import System.IO(hPrint, hPutStr, hPutStrLn, stderr)
import Data.Binary
import System.Exit(exitFailure, exitSuccess)
import Control.Monad(when)

import Data.STAR.Coords(Coord(..), parse)

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
            Left  err    -> do hPutStr   stderr $ "Error parsing " ++ input ++ ": "
                               hPutStrLn stderr $ err
                               exitFailure
            Right parsed -> do case output of
                                 "-" -> print parsed
                                 _   -> do putStrLn $ concat ["Parsed ", show $ length parsed, " coordinate rows."]
                                           Data.Binary.encodeFile output (parsed :: [Coord])
                               exitSuccess
