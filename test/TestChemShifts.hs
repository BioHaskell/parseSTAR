{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module Main(main)
where

import Data.STAR.ChemShifts(ChemShift(..), parse)
import System.Environment(getArgs)
import System.IO(hPrint, hPutStr, hPutStrLn, stderr)
import Data.Binary
import System.Exit(exitFailure, exitSuccess)


main = do [input, output] <- getArgs
          print "A"
          dat <- parse input
          print "B"
          case dat of
            Left  err    -> do hPutStr stderr $ "Error parsing " ++ input ++ ": "
                               print "C1"
                               hPutStrLn stderr $ err
                               print "D1"
                               exitFailure
            Right parsed -> do Data.Binary.encodeFile output (parsed :: [ChemShift])
                               print "C2"
                               exitSuccess
