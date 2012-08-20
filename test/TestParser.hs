module Main(main) where

import Data.STAR.Parser
import Data.STAR.Type
import System.Environment(getArgs)
import Control.Monad((>=>))

main = do args <- getArgs
          mapM (parseFile >=> print) args

