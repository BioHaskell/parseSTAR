module Main(main) where

import Data.STAR.Parser(parseFile)
import Data.STAR.Type
import System.Environment(getArgs)
import Data.Binary(encodeFile)
import System.IO(hPrint, hPutStrLn, stderr)

main = do [input, output] <- getArgs
          dat <- parseFile input
          case dat of
            Right dat -> encodeFile output (dat :: Data.STAR.Type.STAR)
            Left  err -> do hPutStrLn stderr $ "Error parsing " ++ input ++ ": "
                            hPrint stderr err

