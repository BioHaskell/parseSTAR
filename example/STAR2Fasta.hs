{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import           System.IO             (putStrLn, hPutStrLn, hPrint, stderr)
import           System.Environment    (getArgs)
import           Control.Monad         (forM, when)
import           Data.List             (intercalate)
import qualified Data.ByteString.Char8 as BS

import           Data.STAR.ChemShifts  (parse, ChemShift(..),
                                        extractSequenceFromChemShifts,
                                        showSequenceWithChain)

main = do fnames <- getArgs
          when (null fnames) $ do
            putStrLn "Usage: STAR2Fasta <input.str> ..."
            putStrLn "Shows FASTA sequence read from NMR-STAR file."
          forM fnames $ \fname ->
            do putStr $ concat [">", fname, "\n"]
               cs <- Data.STAR.ChemShifts.parse fname
               either (hPutStrLn stderr)
                      (putStrLn                          .
                       intercalate "\n"                  .
                       map (showSequenceWithChain fname) .
                       extractSequenceFromChemShifts     ) cs


