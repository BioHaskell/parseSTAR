{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Data.STAR.ChemShifts as CS
import Text.Printf(hPrintf)
import System.IO(IOMode(WriteMode), stderr, withFile, hPutStrLn)
import System.Environment(getArgs)
import Control.Monad(forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe(catMaybes)

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
-- TODO: later convert to builder?
-- TODO: later emit sequence
header = "VARS   RESID RESNAME ATOMNAME SHIFT\nFORMAT %4d %1s %4s %8.3f"

printTBL cs filename = withFile filename WriteMode $ \outh ->
                         do hPutStrLn outh header
                            forM_ cs $ printRec outh
  where
    printRec outh (ChemShift { seq_id    = resid  ,
                               comp_id   = resname,
                               atom_id   = atname ,
                               chemshift = cs     ,
                               sigma     = sigma  }) = hPrintf outh "%4d %1s %4s %8.3f\n" resid (BS.unpack resname) (BS.unpack atname) cs

reindex i cs = catMaybes . map (reindexRecord i) $ cs
  where
    reindexRecord i cs@(ChemShift { seq_id = resid }) = if seq_id cs + i > 0
                                                          then Just $ cs { seq_id = seq_id cs + i }
                                                          else Nothing

main = do [input, offsetString, output] <- getArgs
          result <- CS.parse input
          let offset :: Int = read offsetString
          case result of
            Left error -> hPutStrLn stderr error
            Right cs   -> do hPrintf stderr "Read %d records.\n" $ Prelude.length cs
                             let cs2 = reindex offset cs
                             printTBL cs2 output

