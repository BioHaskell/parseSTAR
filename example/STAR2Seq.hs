{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           System.IO             (putStrLn,hPrint,stderr)
import           System.Environment    (getArgs)
import           Control.Monad         (forM, when)
import qualified Data.ByteString.Char8 as BS

import           Data.STAR.ResidueCodes
import           Data.STAR.ChemShifts  (parse, ChemShift(..))

extractSequenceFromChemShifts :: [ChemShift] -> [(BS.ByteString, Int)]
extractSequenceFromChemShifts cs = map extract cs
  where
    extract cs = (comp_id cs, seq_id cs)

main = do fnames <- getArgs
          when (null fnames) $ do
            putStrLn "Usage: CheckSeq <input.str> ..."
            putStrLn "Shows FASTA sequence read from NMR-STAR file."
          forM fnames $ \fname ->
            do putStr $ fname ++ ": "
               cs <- Data.STAR.ChemShifts.parse fname
               either (hPrint stderr                        )
                      (print . extractSequenceFromChemShifts) cs

