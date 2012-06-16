{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Prelude
import Data.ByteString.Char8 as BS
import Tokens
--import Text.Show.ByteString
import Data.Binary.Put as Put

--import qualified Text.Show.ByteString as BSS

--instance BSS.Show Token
--  where showp a = Put.putByteString (BS.pack (Prelude.show a))

--(++) = BS.append

myScanTokens str = go (initState str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF                                -> return []
                AlexError ((AlexPn _ line column),_,_) -> fail $ "lexical error at " ++ Prelude.show line ++ " line, " ++ Prelude.show column ++ " column"
                AlexSkip  inp' len                     -> go inp'
                AlexToken inp' len act                 -> do print $ act pos (BS.take (fromIntegral len) str)
                                                             go inp'
-- Test
main = do
  s <- BS.getContents
  myScanTokens s
