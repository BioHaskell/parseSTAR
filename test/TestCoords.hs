{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module Main(main)
where

import Data.STAR.Coords(Coord(..), parse)
import System.Environment(getArgs)
import System.IO(hPrint, hPutStr, stderr)
import Data.Binary
import System.Exit(exitFailure, exitSuccess)


main = do [input, output] <- getArgs
          dat <- parse input
          case dat of
            Left  err    -> do hPutStr stderr $ "Error parsing " ++ input ++ ": "
                               hPrint  stderr $ err
                               exitFailure
            Right parsed -> do Data.Binary.encodeFile output (parsed :: [Coord])
                               print parsed
                               exitSuccess
