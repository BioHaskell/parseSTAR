{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module Data.STAR.Coords(Coord(..), extractCoords, parse)
where

import Prelude hiding(String)
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Nums.Careless.Float as F
import Data.ByteString.Nums.Careless.Int   as I
import Data.Binary
import Control.DeepSeq(NFData(..))
import Data.STAR.Parser(parseFile)
import Data.STAR.Type
import Data.STAR.Path

data Coord = Coord { model_id  :: !Int,
                     res_id    :: !Int,
                     resname   :: !String,
                     atom_id   :: !Int,
                     atom_type :: !String,
                     x         :: !Float,
                     y         :: !Float,
                     z         :: !Float,
                     x_sigma   :: !Float,
                     y_sigma   :: !Float,
                     z_sigma   :: !Float,
                     entry_id  :: !String }
  deriving (Eq, Ord, Show)

{-!
deriving instance Binary Coord
deriving instance NFData Coord
!-}
                             
extractCoords (STAR l) = concatMap extract' l
  where
    extract' (Global l) = []
    extract' (Data _ l) = concatMap coordFrame l

coordFrame (Frame name elts) | frameCategory elts == "conformer_family_coord_set" = concatMap coordLoop elts
  where
    frameCategory elts = emptyHead $ elts ->// entriesByName "Conformer_family_coord_set.Sf_category" ./ entryValue
    emptyHead [] = ""
    emptyHead l  = head l
coordFrame _                                                                      = []

coordLoop (Loop elts@(((Entry e v:_):_))) | "Atom_site.Assembly_ID" `BSC.isPrefixOf` e = map extractCoord elts
coordLoop _                                                                            = []

emptyCoord = Coord { model_id  = maxBound,
                     res_id    = maxBound,
                     resname   = "<UNKNOWN RESIDUE>",
                     atom_id   = maxBound,
                     atom_type = "<UNKNOWN ATOM>",
                     x         = 99e99,
                     y         = 99e99,
                     z         = 99e99,
                     x_sigma   = 99e99,
                     y_sigma   = 99e99,
                     z_sigma   = 99e99,
                     entry_id  = "<UNKNOWN ENTRY>" }

isFilledCoord cs = and . map (\f -> f cs) $ [is_good model_id,
                                             is_good res_id,
                                             is_good resname,
                                             is_good atom_id,
                                             is_good atom_type,
                                             is_good x,
                                             is_good y,
                                             is_good z,
                                             is_good x_sigma,
                                             is_good y_sigma,
                                             is_good z_sigma,
                                             is_good entry_id]
{-((cs_id     cs != cs_id     emptyCoord) &&
                        (seq_id    cs != seq_id    emptyCoord) &&
                        (atom_type cs != atom_type emptyCoord) &&
                        (isotope   cs != isotope   emptyCoord) &&-}
 where
   is_good :: (Eq a) => (Coord -> a) -> Coord -> Bool
   is_good f cs = f cs /= f emptyCoord

compose = foldl apply
  where
    apply e f = f e

extractCoord :: [STAREntry] -> Coord
extractCoord entries = if isFilledCoord entry
                             then entry
                             else error $ "Cannot fill entry from: " ++ show entries
  where
    entryUpdate (Entry "Atom_site.Model_ID"            v) cs = cs{ model_id  = I.int   v }
    entryUpdate (Entry "Atom_site.Label_comp_index_ID" v) cs = cs{ res_id    = I.int   v }
    entryUpdate (Entry "Atom_site.Label_comp_ID"       v) cs = cs{ resname   =         v }
    entryUpdate (Entry "Atom_site.Label_atom_ID"       v) cs = cs{ atom_id   = I.int   v }
    entryUpdate (Entry "Atom_site.Type_symbol"         v) cs = cs{ atom_type =         v }
    entryUpdate (Entry "Atom_site.Cartn_x"             v) cs = cs{ x         = F.float v }
    entryUpdate (Entry "Atom_site.Cartn_y"             v) cs = cs{ y         = F.float v }
    entryUpdate (Entry "Atom_site.Cartn_z"             v) cs = cs{ z         = F.float v }
    entryUpdate (Entry "Atom_site.Cartn_x_esd"         v) cs = cs{ x_sigma   = F.float v }
    entryUpdate (Entry "Atom_site.Cartn_y_esd"         v) cs = cs{ y_sigma   = F.float v }
    entryUpdate (Entry "Atom_site.Cartn_z_esd"         v) cs = cs{ z_sigma   = F.float v }
    entryUpdate (Entry "Atom_site.Entry_ID"            v) cs = cs{ entry_id  =         v }
    entryUpdate _                                         cs = cs -- nothing changed
    updates :: [Coord -> Coord] = map entryUpdate entries
    entry :: Coord = compose emptyCoord updates

parse input = do dat <- Data.STAR.Parser.parseFile input
                 return $ case dat of
                            Right parsed -> Right $ extractCoords parsed
                            Left  e      -> Left e
 
{-
   loop_
      _Atom_site.Assembly_ID
      _Atom_site.Model_ID
      _Atom_site.Model_site_ID

      _Atom_site.ID
      _Atom_site.Assembly_atom_ID
      _Atom_site.Label_entity_assembly_ID
      _Atom_site.Label_entity_ID

      _Atom_site.Label_comp_index_ID
      _Atom_site.Label_comp_ID
      _Atom_site.Label_atom_ID
      _Atom_site.Type_symbol

      _Atom_site.Cartn_x
      _Atom_site.Cartn_y
      _Atom_site.Cartn_z
      _Atom_site.Cartn_x_esd
      _Atom_site.Cartn_y_esd
      _Atom_site.Cartn_z_esd

      _Atom_site.Occupancy
      _Atom_site.Occupancy_esd
      _Atom_site.Uncertainty
      _Atom_site.Ordered_flag
      _Atom_site.Footnote_ID
      _Atom_site.PDBX_label_asym_ID
      _Atom_site.PDBX_label_seq_ID
      _Atom_site.PDBX_label_comp_ID
      _Atom_site.PDBX_label_atom_ID
      _Atom_site.PDBX_formal_charge
      _Atom_site.PDBX_label_entity_ID
      _Atom_site.PDB_record_ID
      _Atom_site.PDB_model_num
      _Atom_site.PDB_strand_ID
      _Atom_site.PDB_ins_code
      _Atom_site.PDB_residue_no
      _Atom_site.PDB_residue_name
      _Atom_site.PDB_atom_name
      _Atom_site.Auth_entity_assembly_ID
      _Atom_site.Auth_asym_ID
      _Atom_site.Auth_chain_ID
      _Atom_site.Auth_seq_ID
      _Atom_site.Auth_comp_ID
      _Atom_site.Auth_atom_ID
      _Atom_site.Auth_alt_ID
      _Atom_site.Auth_atom_name
      _Atom_site.Details
      _Atom_site.Entry_ID
      _Atom_site.Conformer_family_coord_set_ID

      .  1 .    1 . 1 1  1 LYS C    C  -5.326  27.082  -6.102 . . . 1.0 . . . . . . . . . . . . X .  1 LYS C    . . . . . . . . . c17438_2l96 1 
      .  1 .    2 . 1 1  1 LYS CA   C  -5.079  27.607  -7.510 . . . 1.0 . . . . . . . . . . . . X .  1 LYS CA   . . . . . . . . . c17438_2l96 1 
      .  1 .   25 . 1 1  2 LYS C    C  -5.856  23.694  -4.903 . . . 1.0 . . . . . . . . . . . . X .  2 LYS C    . . . . . . . . . c17438_2l96 1 
      .  1 .   26 . 1 1  2 LYS CA   C  -6.026  25.200  -4.740 . . . 1.0 . . . . . . . . . . . . X .  2 LYS CA   . . . . . . . . . c17438_2l96 1 
      .  1 .   27 . 1 1  2 LYS CB   C  -7.407  25.546  -4.177 . . . 1.0 . . . . . . . . . . . . X .  2 LYS CB   . . . . . . . . . c17438_2l96 1 
      .  1 .   28 . 1 1  2 LYS CD   C  -8.970  25.425  -2.197 . . . 1.0 . . . . . . . . . . . . X .  2 LYS CD   . . . . . . . . . c17438_2l96 1 
      .  1 .   29 . 1 1  2 LYS CE   C -10.181  25.325  -3.117 . . . 1.0 . . . . . . . . . . . . X .  2 LYS CE   . . . . . . . . . c17438_2l96 1 
      .  1 .   30 . 1 1  2 LYS CG   C  -7.704  24.880  -2.844 . . . 1.0 . . . . . . . . . . . . X .  2 LYS CG   . . . . . . . . . c17438_2l96 1 
      .  7 . 2838 . 1 1 15 LYS HD2  H   3.549  -4.552  -4.566 . . . 1.0 . . . . . . . . . . . . X . 15 LYS HD2  . . . . . . . . . c17438_2l96 1 
      .  7 . 2839 . 1 1 15 LYS HD3  H   5.219  -5.118  -4.629 . . . 1.0 . . . . . . . . . . . . X . 15 LYS HD3  . . . . . . . . . c17438_2l96 1 
      .  7 . 2840 . 1 1 15 LYS HE2  H   3.081  -6.054  -2.720 . . . 1.0 . . . . . . . . . . . . X . 15 LYS HE2  . . . . . . . . . c17438_2l96 1 
      .  7 . 2841 . 1 1 15 LYS HE3  H   3.709  -6.947  -4.104 . . . 1.0 . . . . . . . . . . . . X . 15 LYS HE3  . . . . . . . . . c17438_2l96 1 
      .  7 . 2842 . 1 1 15 LYS HG2  H   5.522  -4.307  -2.297 . . . 1.0 . . . . . . . . . . . . X . 15 LYS HG2  . . . . . . . . . c17438_2l96 1 
      .  7 . 2843 . 1 1 15 LYS HG3  H   3.896  -3.623  -2.367 . . . 1.0 . . . . . . . . . . . . X . 15 LYS HG3  . . . . . . . . . c17438_2l96 1 
      . 10 . 4288 . 1 1 24 ALA N    N  18.192   1.376   1.791 . . . 1.0 . . . . . . . . . . . . X . 24 ALA N    . . . . . . . . . c17438_2l96 1 
      . 10 . 4289 . 1 1 24 ALA O    O  20.909   2.034   3.478 . . . 1.0 . . . . . . . . . . . . X . 24 ALA O    . . . . . . . . . c17438_2l96 1 
      . 10 . 4290 . 1 1 24 ALA OXT  O  19.164   1.437   4.669 . . . 1.0 . . . . . . . . . . . . X . 24 ALA OXT  . . . . . . . . . c17438_2l96 1 
-}
