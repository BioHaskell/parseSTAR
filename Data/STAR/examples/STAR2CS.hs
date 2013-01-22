module Main(main) where

import Data.STAR.ChemShifts as CS
import Text.Printf(hPrintf)
import System.IO(IOMode(WriteMode), hPrint, stderr, withFile)
import System.Environment(getArgs)
import Control.Monad(forM_)
import Data.ByteString.Char8 as BS

{- Output format:
DATA SEQUENCE GR NSAKDIRTEERARVQLGNVVT AAAL GSGSGSGSGSGS
DATA SEQUENCE TT NSVETVVGKGESRVLIGNEYG GKGF GSGSGSGSGSGS
DATA SEQUENCE GR NSAKDIRTEERARVQLGNVVT AAAL GSGSGSGSGSGS
DATA SEQUENCE TT NSVETVVGKGESRVLIGNEYG GKGF GSGSGSGSGSGS
DATA SEQUENCE GR NSAKDIRTEERARVQLGNVVT AAAL 

VARS   RESID RESNAME ATOMNAME SHIFT
FORMAT %4d %1s %4s %8.3f

    1 G   CA   43.791
    1 G    N  109.800
 -}
{- 
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
 -}
-- TODO: later convert to builder?
-- TODO: later emit sequence
header = "VARS   RESID RESNAME ATOMNAME SHIFT\nFORMAT %4d %1s %4s %8.3f"
printTBL cs filename = withFile filename WriteMode $ \outh ->
                         do hPrint outh header
                            forM_ cs $ printRec outh
  where
    printRec outh (ChemShift { seq_id    = resid  ,
                               comp_id   = resname,
                               atom_id   = atname ,
                               chemshift = cs     ,
                               sigma     = sigma  }) = hPrintf outh "%4d %1s %4s %8.3f\n" resid (BS.unpack resname) (BS.unpack atname) cs

main = do [input, output] <- getArgs
          result <- CS.parse input
          case result of
            Left error -> hPrint stderr error
            Right cs   -> do hPrintf stderr "Read %d records" $ Prelude.length cs
                             printTBL cs output

