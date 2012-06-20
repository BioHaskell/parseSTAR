{-# LANGUAGE ScopedTypeVariables #-}
module Main(main)
where

import Data.Binary(decodeFile, encodeFile)
import Data.STAR.ChemShifts
import System.Environment(getArgs)
import Control.Monad(forM)

main = do args <- getArgs
          (lists :: [[ChemShift]]) <- forM args decodeFile
          encodeFile "total.cs" (Prelude.concat lists)


