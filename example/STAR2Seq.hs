{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import           System.IO             (putStrLn, hPutStrLn, hPrint, stderr)
import           System.Environment    (getArgs)
import           Control.Monad         (forM, when)
import           Data.List             (nub, intercalate, groupBy)
import qualified Data.ByteString.Char8 as BS

import           Data.STAR.ResidueCodes
import           Data.STAR.ChemShifts  (parse, ChemShift(..))

extractSequenceFromChemShifts :: [ChemShift] -> [Char]
extractSequenceFromChemShifts = intercalate "*"            .
                                map (map $ seqCode . fst3) .
                                groupBy third              .
                                fillGaps                   .
                                nub                        .
                                map extract
  where
    extract cs = (comp_id cs, seq_id cs, entity_id cs)
    fillGaps ((a,i,e):(b,j,f):rs) |     e /= f = (a,i,e):fillGaps (            (b,j,f):rs)
    fillGaps ((a,i,e):(b,j,f):rs) | i + 1 >= j = (a,i,e):fillGaps (            (b,j,f):rs)
    fillGaps ((a,i,e):(b,j,f):rs)              = (a,i,e):fillGaps (("-",i+1,e):(b,j,f):rs)
    fillGaps                  rs               = rs
    third (_, _, a) (_, _, b) = a == b
    fst3    (a, _, _) = a
    seqCode "-"       = '-'
    seqCode x         = toSingleLetterCode x
        
    -- TODO: check monotonicity of sequence numbers.
    -- TODO: UNK -> '-'

main = do fnames <- getArgs
          when (null fnames) $ do
            putStrLn "Usage: STAR2Seq <input.str> ..."
            putStrLn "Shows FASTA sequence read from NMR-STAR file."
          forM fnames $ \fname ->
            do putStr $ concat [">", fname, "\n"]
               cs <- Data.STAR.ChemShifts.parse fname
               either (hPutStrLn stderr                        )
                      (putStrLn . extractSequenceFromChemShifts) cs

