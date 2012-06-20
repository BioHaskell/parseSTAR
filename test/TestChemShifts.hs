{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module Main(main)
where

import Data.STAR.ChemShifts(ChemShift(..), parse)
import System.Environment(getArgs)
import System.IO(hPrint, stderr)
import Data.Binary


main = do [input, output] <- getArgs
          dat <- parse input
          case dat of
            Left  err    -> hPrint stderr $ err
            Right parsed -> Data.Binary.encodeFile output (parsed :: [ChemShift])
