{-# LANGUAGE DisambiguateRecordFields #-}
module Data.STAR(Data.STAR.Type.STAR(..),
                 Data.STAR.Type.STARBlock(..),
                 Data.STAR.Type.STAREntry(..),
                 Data.STAR.Type.STARKey(..),
                 parseSTAR,
                 parseSTARFile,
                 Data.STAR.ChemShifts.extractChemShifts,
                 Data.STAR.Coords.extractCoords) where

import qualified Data.STAR.Parser(parse, parseFile)
import Data.STAR.Type
import Data.STAR.ChemShifts(extractChemShifts, ChemShift(..))
import Data.STAR.Coords    (extractCoords,     Coord    (..))

parseSTAR         = Data.STAR.Parser.parse
parseSTARFile     = Data.STAR.Parser.parseFile


