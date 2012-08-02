{-# LANGUAGE OverloadedStrings #-} 
module Main(main) where

import Data.STAR.Parser
import Data.STAR.Type
import Data.STAR.Path -- (allEntriesByName)
import System.Environment(getArgs)

main = do args <- getArgs
          mapM (\fname -> parseFile fname >>= either print (print . aPath)) args

aPath = allEntriesByName "Entity.Polymer_seq_one_letter_code"
