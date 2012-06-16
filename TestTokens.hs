{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Prelude
import Data.ByteString.Char8 as BS
import Tokens

--(++) = BS.append

myScanTokens str = go (initState str)
  where go (inp@(pos,_,str), state) =
          case alexScan inp state of
                AlexEOF                                -> return []
                AlexError ((AlexPn _ line column),_,_) -> fail $ "lexical error at " ++ Prelude.show line ++ " line, " ++ Prelude.show column ++ " column, state " ++ show state
                AlexSkip  inp' len                     -> go (inp', state)
                AlexToken inp' len (act, state')       -> do print $ act pos (BS.take (fromIntegral len) str)
                                                             go (inp', state')
-- Test
main = do s <- BS.getContents
          myScanTokens s
