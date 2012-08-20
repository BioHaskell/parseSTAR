{-# LANGUAGE OverloadedStrings #-} 
module Main(main) where

import Data.STAR.Parser
import Data.STAR.Type
import Data.STAR.Path -- (allEntriesByName)
import System.Environment(getArgs)
import Control.Monad((>=>))

main = do args <- getArgs
          mapM (parseFile >=> either print (print . aPath)) args

aPath = allEntriesByName "Entity.Polymer_seq_one_letter_code"
