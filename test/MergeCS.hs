{-# LANGUAGE ScopedTypeVariables #-}
module Main(main)
where

import Data.Binary(decodeFile, encodeFile)
import Data.STAR.ChemShifts
import System.Environment(getArgs)
import System.IO(hPutStrLn, hPrint, stderr, putStrLn)
import Control.Monad(forM)
import Control.DeepSeq(rnf)
import Control.Exception

robustDecode fname = (do (r :: [ChemShift]) <- decodeFile fname
                         r `seq` putStrLn $ "Success: " ++ fname
                         return r)
                      `Control.Exception.catch`
                     (\(e :: SomeException)-> do hPutStrLn stderr $ "Error in " ++ fname ++ ": " ++ show e
                                                 return [])



main = do args <- getArgs
          (lists :: [[ChemShift]]) <- forM args robustDecode
          encodeFile "total.cs" $ Prelude.concat lists


