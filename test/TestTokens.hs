{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Prelude
import Data.ByteString.Char8 as BS
import Data.STAR.Tokens as Tokens
import Data.STAR.StringUtil(simpleRead)
import System.Environment(getArgs)

myScanTokens str = go (initState str)
  where go (inp@(pos,_,str), state) =
          case alexScan inp state of
                AlexEOF                                -> return []
                AlexError ((AlexPn _ line column),_,_) -> fail $ "lexical error at " ++ Prelude.show line ++ " line, " ++ Prelude.show column ++ " column, state " ++ show state
                AlexSkip  inp' len                     -> go (inp', state)
                AlexToken inp' len (act, state')       -> do print $ act pos (BS.take (fromIntegral len) str)
                                                             go (inp', state')
-- Test
main = do args <- getArgs
          mapM (\fname ->
                do s <- simpleRead fname
                   myScanTokens s) args
