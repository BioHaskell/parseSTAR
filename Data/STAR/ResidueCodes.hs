{-# LANGUAGE OverloadedStrings #-}
module Data.STAR.ResidueCodes(allPDBAminoacids
                             ,allFASTAAminoacids
                             ,toSingleLetterCode
                             ,maybeToSingleLetterCode
                             ,toThreeLetterCode      )
where

import Prelude hiding(String)
import Data.Array
import Data.Map hiding((!))
import qualified Data.List(zipWith, zip)
import Data.ByteString.Char8 as BSC

-- | Three-letter codes for standard aminoacids
stdAaTLC :: Array Int BSC.ByteString
stdAaTLC = listArray (0, Prelude.length tlcList) tlcList
  where tlcList = [ "ALA", "CYS", "ASP", "GLU", "PHE"
                  , "GLY", "HIS", "ILE", "LYS", "LEU"
                  , "MET", "ASN", "PRO", "GLN", "ARG"
                  , "SER", "THR", "VAL", "TRP", "TYR" ]

allPDBAminoacids   :: [BSC.ByteString]
allPDBAminoacids   = Data.Array.elems stdAaTLC
allFASTAAminoacids :: [Char]
allFASTAAminoacids = BSC.unpack stdAaSLC

-- | FASTA codes for standard aminoacids
stdAaSLC ::BSC.ByteString
stdAaSLC = BSC.pack "ACDEFGHIKLMNPQRSTVWY"

-- | Finds a three-letter PDB/BMRB aminoacid code for a given single-letter FASTA code
--   (or returns "UNK" for unknown.)
toThreeLetterCode :: Char -> ByteString
toThreeLetterCode c = case BSC.elemIndex c stdAaSLC of
                        Just p  -> stdAaTLC ! p
                        Nothing -> "UNK"

-- | Finds a single-letter FASTA code for a given three-letter PDB code (or returns 'X'.)
toSingleLetterCode :: ByteString -> Char
toSingleLetterCode c = Data.Map.findWithDefault 'X' c tlcMap

maybeToSingleLetterCode :: ByteString -> Maybe Char
maybeToSingleLetterCode c = Data.Map.lookup c tlcMap 

tlcMap = Data.Map.fromList $ Data.List.zip (Data.Array.elems stdAaTLC) (BSC.unpack stdAaSLC)

stopCode='*'
