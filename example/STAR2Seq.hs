{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import           System.IO             (putStrLn,hPutStrLn,hPrint,stderr)
import           System.Environment    (getArgs)
import           Control.Monad         (forM, when)
import           Data.List             (nub)
import qualified Data.ByteString.Char8 as BS

import           Data.STAR.ResidueCodes
import           Data.STAR.ChemShifts  (parse, ChemShift(..))

extractSequenceFromChemShifts :: [ChemShift] -> [Char]
extractSequenceFromChemShifts = map (seqCode . fst) .
                                fillGaps            .
                                nub                 .
                                map extract
  where
    extract cs = (comp_id cs, seq_id cs)
    fillGaps ((a,i):(b,j):rs) | i + 1 >= j = (a,i):fillGaps (          (b,j):rs)
    fillGaps ((a,i):(b,j):rs)              = (a,i):fillGaps (("-",i+1):(b,j):rs)
    fillGaps rs                            = rs
    seqCode "-" = '-'
    seqCode x   = toSingleLetterCode x
        
    -- TODO: check monotonicity of sequence numbers.
    -- TODO: UNK -> '-'

main = do fnames <- getArgs
          when (null fnames) $ do
            putStrLn "Usage: CheckSeq <input.str> ..."
            putStrLn "Shows FASTA sequence read from NMR-STAR file."
          forM fnames $ \fname ->
            do putStr $ fname ++ ": "
               cs <- Data.STAR.ChemShifts.parse fname
               either (hPutStrLn stderr                        )
                      (putStrLn . extractSequenceFromChemShifts) cs

