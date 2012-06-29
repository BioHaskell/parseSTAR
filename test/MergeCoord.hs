{-# LANGUAGE ScopedTypeVariables #-}
module Main(main)
where

import Data.Binary(decodeFile, encodeFile)
import Data.STAR.Coords
import System.Environment(getArgs)
import System.IO(hPutStrLn, hPrint, stderr, putStrLn)
import Control.Monad(forM)
import Control.DeepSeq(rnf)
import Control.Exception

isFirstModel c = model_id c == 1

robustDecode fname = ((do (r :: [Coord]) <- decodeFile fname
                          let fr = filter isFirstModel r
                          rnf fr `seq` putStrLn $ "Success: " ++ fname
                          return fr)
                       `Control.Exception.catch`
                      (\(e :: SomeException)-> do hPutStrLn stderr $ "Error in " ++ fname ++ ": " ++ show e
                                                  return []))

main = do args <- getArgs
          (lists :: [[Coord]]) <- forM args robustDecode
          encodeFile "total.crd" (Prelude.concat lists)


