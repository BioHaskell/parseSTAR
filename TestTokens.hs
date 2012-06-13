module Main(main) where

import Data.ByteString.Lazy.Char8 as BS
import Tokens

myScanTokens str = go (initState str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> do print $ act pos (BS.take (fromIntegral len) str) 
                                             go inp'
-- Test
main = do
  s <- BS.getContents
  myScanTokens s
