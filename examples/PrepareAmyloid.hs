#!/usr/bin/runghc
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Main(main) where

import Data.STAR.ChemShifts as CS
import Text.Printf(hPrintf)
import System.IO(IOMode(WriteMode), stderr, withFile, hPutStrLn, stdout)
import Control.Monad(forM_, when)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe(catMaybes)

import Data.Typeable
import System.Console.GetOpt
import System.Environment(getProgName, getArgs)
import System.Exit(exitWith, exitFailure, ExitCode(..))
import Prelude

data Options = Options  { optVerbosity :: Int,
                          cutStart     :: Int,
                          cutEnd       :: Int,
                          offset       :: Int
                        }
defaultOptions = Options { optVerbosity = 0,
                           cutStart     = -99999999, -- TODO: is there inf :: Int?
                           cutEnd       =  99999999,
                           offset       = 0
                         }

showHelp :: IO ()
showHelp =
        do prg <- getProgName
           hPutStrLn stdout (usageInfo prg options)
           exitFailure

showVersion = hPutStrLn stdout "Version 0.5"

exitAfter function exitCode =
  \opts -> do function
              exitWith exitCode
              return opts

changeVerbosity :: (Int -> Int) -> Options -> IO Options
changeVerbosity aChange = \opt -> return opt { optVerbosity = aChange (optVerbosity opt) }

intOpt :: (Int -> Options -> Options) -> String -> Options -> IO Options

{-intOpt :: forall a. (ReadS    a,
                     Typeable a) => (a -> Options -> Options) ->
                                    String  ->
                                    Options -> IO Options-}
intOpt setter input opts = do case rest of
                                ""        -> return $ setter x opts
                                otherwise -> do hPrintf stderr
                                                        "Error parsing option with %s argument: '%s'"
                                                        (show $ typeOf x)
                                                        input
                                                exitFailure
  where
    ((x, rest): _) = reads input

options  :: [OptDescr (Options -> IO Options)]
options = 
  [-- processing options
   Option ['s'] ["cut-start-to"]  (ReqArg (intOpt $ \a opt -> opt { cutStart = a }) "start" )
           "Cuts residues up to (and including) given number.",
   Option ['e'] ["cut-tail-from"] (ReqArg (intOpt $ \a opt -> opt { cutStart = a }) "end")
           "Cuts residues from (and including) given number.",
   Option ['o'] ["offset"]        (ReqArg (intOpt $ \a opt -> opt { offset   = a }) "offset")
           "Adds a given offset to all residue numbers (may be negative.)",
   -- log options
   Option ['v'] ["verbose"] (NoArg (changeVerbosity (\a -> a + 1)))
           "Increases log verbosity.",
   Option ['q'] ["quiet"]   (NoArg (changeVerbosity (\a -> a - 1)))
           "Decreases log verbosity.",
   -- informational
   Option ['V'] ["version"] (NoArg (exitAfter showVersion ExitSuccess))
           "Print program version.",
   Option ['h'] ["help"]    (NoArg (exitAfter showHelp    ExitSuccess))
           "Prints help"
  ]

-- TODO: generate sequence
-- TODO: add cutting all before Nth, and after Nth residue.
-- TODO: add CLI option parsing from template
-- TODO: make a general Data.STAR.TBL module for TBL conversion and in/out?
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

reindex i cs = map (reindexRecord i) cs
  where
    reindexRecord i cs@(ChemShift { seq_id = resid }) = cs { seq_id = seq_id cs + i }

cut start end cs = filter isWithin cs
  where
    isWithin (ChemShift { seq_id = resid }) = (resid > start) && (resid < end)

main = do (actions, filenames, errors) <- getOpt RequireOrder options `fmap` getArgs
          opts <- foldl (>>=) (return defaultOptions) actions
          when (length filenames /= 2) (showHelp >> exitFailure)
          let [input, output] = filenames
          result <- CS.parse input
          case result of
            Left error -> hPutStrLn stderr error
            Right cs   -> do hPrintf stderr "Read %d records.\n" $ Prelude.length cs
                             hPrintf stderr "Filtering those between %d and %d, offset is %d\n" (cutStart opts) (cutEnd opts) (offset opts)
                             let cs2 = reindex (offset opts) $ cut (cutStart opts) (cutEnd opts) cs
                             printTBL cs2 output
          exitWith ExitSuccess

