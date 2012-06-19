module Main(main) where

import Data.STAR.Parser(parseFile)
import Data.STAR.Type
import System.Environment(getArgs)
import Data.Binary(encodeFile)

main = do [input, output] <- getArgs
          dat <- parseFile input
          encodeFile output dat

