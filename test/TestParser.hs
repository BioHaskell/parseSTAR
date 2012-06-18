module Main(main) where

import Data.STAR.Parser
import Data.STAR.Type
import System.Environment(getArgs)

main = do args <- getArgs
          mapM (\fname -> parseFile fname >>= print) args

