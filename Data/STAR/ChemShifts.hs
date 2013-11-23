{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module Data.STAR.ChemShifts(ChemShift(..)
                           ,extractChemShifts
                           ,parse
                           ,extractSequenceFromChemShifts
                           ,showSequenceWithChain)
where

import           Prelude                             hiding(String)
import qualified Data.ByteString.Char8               as BSC
import qualified Data.Binary                         as B
import           Data.ByteString.Nums.Careless.Float as F
import           Data.ByteString.Nums.Careless.Int   as I
import           Data.Binary           (Binary(..))
import           Control.DeepSeq       (NFData(..))
import           Data.Typeable
import           Control.Monad.Trans   (lift)
import           Control.Arrow         ((&&&))
import           Data.List             (groupBy, nub)

import           Data.STAR.Parser      (parseFile)
import           Data.STAR.Type
import           Data.STAR.Path
import           Data.STAR.ResidueCodes(toSingleLetterCode)

-- | Record representing single chemical shift.
data ChemShift = ChemShift { cs_id     :: !Int,
                             seq_id    :: !Int,
                             entity_id :: !Int,
                             comp_id   :: !String,
                             atom_id   :: !String,
                             atom_type :: !String,
                             isotope   :: !Int,
                             chemshift :: !Float,
                             sigma     :: !Float,
                             entry_id  :: !String
                           }
  deriving (Eq, Ord, Show, Typeable)

{-!
deriving instance Binary ChemShift
deriving instance NFData ChemShift
!-}

-- | Extracts chemical shift list from a STAR file contents.
extractChemShifts ::  STAR -> [ChemShift]
extractChemShifts (STAR l) = concatMap extract' l
  where
    extract' (Global l) = []
    extract' (Data _ l) = concatMap chemShiftFrame l

-- | Go down STAREntry and extract frame with chemical shifts.
chemShiftFrame ::  STAREntry -> [ChemShift]
chemShiftFrame (Frame name elts) | frameCategory elts == "assigned_chemical_shifts" = concatMap chemShiftLoop elts
  where
    frameCategory elts = emptyHead $ elts ->// entriesByName "Assigned_chem_shift_list.Sf_category" ./ entryValue
    emptyHead [] = ""
    emptyHead l  = head l
chemShiftFrame _                                                                    = []

-- | Maps an NMR-STAR loop with chemical shifts into ChemShift records.
chemShiftLoop ::  STAREntry -> [ChemShift]
chemShiftLoop (Loop elts@((Entry e v:_):_)) | "Atom_chem_shift" `BSC.isPrefixOf` e = map extractChemShift elts
chemShiftLoop _                                                                    = []

-- | Empty ChemShift record template.
-- All field values correspond to "undefined" values,
-- so it can be later compared with any filled record.
--
-- In this particular case it seems to work better than making everything
-- Maybe.
emptyChemShift ::  ChemShift
emptyChemShift = ChemShift { cs_id     = -1,
                             entity_id = -1,
                             seq_id    = -1,
                             comp_id   = "<UNKNOWN COMPOUND>",
                             atom_id   = "<UNKNOWN ID>",
                             atom_type = "<UNKNOWN TYPE>",
                             isotope   = -1,
                             chemshift = -999.999,
                             sigma     = -999.999,
                             entry_id  = "<UNKNOWN ENTRY>" }

-- | Does this ChemShift record contain all expected components?
isFilledChemShift ::  ChemShift -> Bool
isFilledChemShift cs = all (\f -> f cs) [is_good cs_id,
                                         is_good seq_id,
                                         is_good comp_id,
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

-- | Extracts ChemShift records from a list of STAR entries.
extractChemShift :: [STAREntry] -- parsed STAR file
                 -> ChemShift
extractChemShift entries = if isFilledChemShift entry
                             then entry
                             else error $ "Cannot fill entry from: " ++ show entries
  where
    entryUpdate (Entry "Atom_chem_shift.ID"                  v) cs = cs { cs_id     = I.int v   }
    entryUpdate (Entry "Atom_chem_shift.Entity_ID"           v) cs = cs { entity_id = I.int v   }
    entryUpdate (Entry "Atom_chem_shift.Seq_ID"              v) cs = cs { seq_id    = I.int v   }
    entryUpdate (Entry "Atom_chem_shift.Atom_ID"             v) cs = cs { atom_id   = v         } -- TODO: hashed string?
    entryUpdate (Entry "Atom_chem_shift.Comp_ID"             v) cs = cs { comp_id   = v         } -- TODO: hashed string?
    entryUpdate (Entry "Atom_chem_shift.Atom_type"           v) cs = cs { atom_type = v         } -- TODO: hashed string?
    entryUpdate (Entry "Atom_chem_shift.Atom_isotope_number" v) cs = cs { isotope   = I.int v   }
    entryUpdate (Entry "Atom_chem_shift.Val"                 v) cs = cs { chemshift = F.float v }
    entryUpdate (Entry "Atom_chem_shift.Val_err"             v) cs = cs { sigma     = F.float v }
    entryUpdate (Entry "Atom_chem_shift.Entry_ID"            v) cs = cs { entry_id  = v         }
    entryUpdate _                                               cs = cs -- nothing changed
    updates :: [ChemShift -> ChemShift] = map entryUpdate entries
    entry :: ChemShift = compose emptyChemShift updates

--NOTE: for aminoacids atom with id "H" is "HN" in PDB nomenclature.
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

-- | Extracts FASTA sequence from a list of chemical shift records.
extractSequenceFromChemShifts :: [ChemShift]     -- ^ list of chemical shift records
                              -> [(Int, [Char])] -- ^ list of chain numbers with their FASTA sequences
extractSequenceFromChemShifts = map (    (trd3    . head) &&&
                                     map (seqCode . fst3)) .
                                groupBy thirdEq            .
                                fillGaps                   .
                                nub                        .
                                map extract
  where
    extract cs = (comp_id   cs,
                  seq_id    cs,
                  entity_id cs)
    fillGaps ((a,i,e):(b,j,f):rs) |     e /= f = (a,i,e):fillGaps (            (b,j,f):rs)
    fillGaps ((a,i,e):(b,j,f):rs) | i + 1 >= j = (a,i,e):fillGaps (            (b,j,f):rs)
    fillGaps ((a,i,e):(b,j,f):rs)              = (a,i,e):fillGaps (("-",i+1,e):(b,j,f):rs)
    fillGaps                  rs               = rs
    thirdEq (_, _, a) (_, _, b) = a == b
    fst3    (a, _, _)         = a
    trd3    (_, _, c)         = c
    seqCode x | BSC.length x == 1 && BSC.head x `BSC.elem` "ACGUT-" = BSC.head x
    seqCode x                                                       = toSingleLetterCode x
    
    -- TODO: check monotonicity of sequence numbers.

-- | Shows FASTA record for a given filename, and chain identifier.
showSequenceWithChain ::       [Char]  -- ^ name of sequence source
                      -> (Int, [Char]) -- ^ chain number and FASTA sequence
                      ->       [Char]  -- ^ result string
showSequenceWithChain fname (chain, seq) = concat [">",
                                                   fname,
                                                   ":",
                                                   show chain,
                                                   "\n",
                                                   seq]


-- | Parse NMR-STAR file and and return either error message, or list of chemical shifts.
parse ::  [Char] -- filename
      -> IO (Either [Char] [ChemShift])
parse input = do dat <- Data.STAR.Parser.parseFile input
                 return $ case dat of
                            Right parsed -> Right $ extractChemShifts parsed
                            Left  e      -> Left e

