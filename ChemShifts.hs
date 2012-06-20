{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main(main)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BSC
import Data.STAR.Parser(parseFile)
import Data.STAR.Type
import Data.ByteString.Nums.Careless.Float as F
import Data.ByteString.Nums.Careless.Int   as I
import System.Environment(getArgs)
import Control.Monad(forM_)
import System.IO(hPrint, stderr)

data ChemShift = ChemShift { cs_id     :: Int,
                             seq_id    :: Int,
                             atom_type :: String,
                             isotope   :: Int,
                             chemshift :: Float,
                             sigma     :: Float,
                             entry_id  :: String
                           }
  deriving (Eq, Ord, Show)
                             
extractChemShifts (STAR l) = concatMap extract' l
  where
    extract' (Global l) = []
    extract' (Data _ l) = concatMap chemShiftFrame l

chemShiftFrame (Frame name elts) | "assigned_chem_shift_list_" `BSC.isPrefixOf` name = concatMap chemShiftLoop elts
chemShiftFrame _                                                                     = []

chemShiftLoop (Loop elts) = map extractChemShift elts
chemShiftLoop _           = []

emptyChemShift = ChemShift { cs_id     = (-1),
                             seq_id    = (-1),
                             atom_type = "<UNKNOWN>",
                             isotope   = (-1),
                             chemshift = (-999.999),
                             sigma     = (-999.999),
                             entry_id  = "<UNKNOWN ENTRY>" }

isFilledChemShift cs = and . map (\f -> f cs) $ [is_good cs_id,
                                                 is_good seq_id,
                                                 is_good atom_type,
                                                 is_good isotope,
                                                 is_good chemshift,
                                                 is_good sigma,
                                                 is_good entry_id]
{-((cs_id     cs != cs_id     emptyChemShift) &&
                        (seq_id    cs != seq_id    emptyChemShift) &&
                        (atom_type cs != atom_type emptyChemShift) &&
                        (isotope   cs != isotope   emptyChemShift) &&-}
 where
   is_good :: (Eq a) => (ChemShift -> a) -> ChemShift -> Bool
   is_good f cs = f cs /= f emptyChemShift

compose = foldl apply
  where
    apply e f = f e

extractChemShift :: [STAREntry] -> ChemShift
extractChemShift entries = entry -- TODO: sanity check (isFilledChemShift)
  where
    entryUpdate (Entry "Atom_chem_shift.Seq_ID" v) cs = cs{seq_id = I.int v}
    entryUpdate _                                  cs = cs -- nothing changed
    updates :: [ChemShift -> ChemShift] = map entryUpdate entries
    entry :: ChemShift = compose emptyChemShift updates


{-
save_assigned_chem_shift_list_1
...
      _Atom_chem_shift.ID
      _Atom_chem_shift.Assembly_atom_ID
      _Atom_chem_shift.Entity_assembly_ID
      _Atom_chem_shift.Entity_ID
      _Atom_chem_shift.Comp_index_ID
      _Atom_chem_shift.Seq_ID

      _Atom_chem_shift.Comp_ID
      _Atom_chem_shift.Atom_ID
      _Atom_chem_shift.Atom_type
      _Atom_chem_shift.Atom_isotope_number

      _Atom_chem_shift.Val
      _Atom_chem_shift.Val_err

      _Atom_chem_shift.Assign_fig_of_merit
      _Atom_chem_shift.Ambiguity_code
      _Atom_chem_shift.Occupancy
      _Atom_chem_shift.Resonance_ID
      _Atom_chem_shift.Auth_entity_assembly_ID
      _Atom_chem_shift.Auth_seq_ID

      _Atom_chem_shift.Auth_comp_ID
      _Atom_chem_shift.Auth_atom_ID
      _Atom_chem_shift.Details
      _Atom_chem_shift.Entry_ID
      _Atom_chem_shift.Assigned_chem_shift_list_ID

         1 . 1 1   1   1 MET HE1  H  1   2.276 0.010 . 1 . . .   1 MET HE1  . c16678_2ksy 1 
         2 . 1 1   1   1 MET HE2  H  1   2.276 0.010 . 1 . . .   1 MET HE2  . c16678_2ksy 1 
         3 . 1 1   1   1 MET HE3  H  1   2.276 0.010 . 1 . . .   1 MET HE3  . c16678_2ksy 1 
         4 . 1 1   1   1 MET C    C 13 176.363 0.000 . 1 . . .   1 MET C    . c16678_2ksy 1 
      2769 . 1 1 241 241 GLU HG3  H  1   2.319 0.000 . 1 . . . 241 GLU HG3  . c16678_2ksy 1 
      2770 . 1 1 241 241 GLU C    C 13 176.745 0.000 . 1 . . . 241 GLU C    . c16678_2ksy 1 
      2771 . 1 1 241 241 GLU CA   C 13  56.773 0.000 . 1 . . . 241 GLU CA   . c16678_2ksy 1 
      2772 . 1 1 241 241 GLU CB   C 13  29.436 0.000 . 1 . . . 241 GLU CB   . c16678_2ksy 1 
      2773 . 1 1 241 241 GLU N    N 15 118.823 0.172 . 1 . . . 241 GLU N    . c16678_2ksy 1 
-}

main = do fnames <- getArgs
          forM_ fnames (\fname -> do dat <- parseFile fname
                                     case dat of
                                       Left  err    -> hPrint stderr $ err
                                       Right parsed -> print $ extractChemShifts parsed)

