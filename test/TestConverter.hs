module Main(main) where

import Data.STAR.Parser(parseFile)
import Data.STAR.Type
import System.Environment(getArgs)
import Data.Binary(encodeFile)
import System.IO(hPrint, hPutStr, hPutStrLn, stderr)
import System.Exit(exitFailure, exitSuccess)

main = do [input, output] <- getArgs
          dat <- parseFile input
          case dat of
            Right dat -> do encodeFile output (dat :: Data.STAR.Type.STAR)
                            exitSuccess
            Left  err -> do hPutStr   stderr $ "Error parsing " ++ input ++ ": "
                            hPutStrLn stderr err
                            exitFailure


