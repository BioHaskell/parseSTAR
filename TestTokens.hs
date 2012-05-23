module Main(main) where

import Tokens

myScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> do print $ act pos (take len str) 
                                             go inp'

-- Test
main = do
  s <- getContents
  let startInput = (AlexPn 0 0 0, '\n', [], s)
  myScanTokens s
