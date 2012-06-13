{-# LANGUAGE OverloadedStrings,BangPatterns #-}
module ParserMonad(Parser(..), ParseResult(..), ParserState(..),
                   parseReturn, parseFail, parseThen,
                   getPos, getInput, setInput,
                   getToken, initState, extractPos, extractInput,
                   STARKey(..), STAREntry(..),
                   STARType(..), STARStruct(..), parseError, 
                   matchTypesValues,
                   savedEntry, deref
                  ) where

import Control.Monad.State

import Prelude hiding (String)
import Tokens
import Type
import Data.ByteString.Lazy.Char8 as BSC

-- Parser Tools
data STARType   = TSimple  STARKey
                | TComplex [STARType]
  deriving (Show,Eq)
data STARStruct = SText Tokens.AlexPosn String -- keep position for matchTypesValues error reporting!
                | SRef  Tokens.AlexPosn String -- TODO: implement!!!
                | SStop Tokens.AlexPosn
  deriving (Show,Eq)

matchTypesValues  :: [STARType] -> [STARStruct] -> STAREntry
matchTypesValues ts ss = r
  where ([], r)        = matchTypesValues' ts ss

matchTypesValues' :: [STARType] -> [STARStruct] -> ([STARStruct], STAREntry)
matchTypesValues' ts ss = (ss', Type.Loop r)
  where (ss', r ) = match' ts ss
        match' :: [STARType] -> [STARStruct] -> ([STARStruct], [STAREntry])
        match' []               (SStop p  :ss) = (ss, [])
        match' []               ss             = match' ts ss
        match' (TSimple  t :ts) (SText p s:ss) = let (ss',                r) = match'    ts ss
                                                 in  (ss', Type.Entry t s:r)
        match' (TSimple  t :ts) (SRef  p s:ss) = let (ss',                r) = match'    ts ss
                                                 in  (ss', Type.Ref   t s:r)
        match' (TComplex tc:ts) []             = error $ "Cannot find any more values to match " ++ show (TComplex tc)
        match' (TComplex tc:ts) ss             = let (ss' , lr  ) = matchTypesValues' tc ss
                                                     (ss'',    r) = match'            ts ss'
                                                 in  (ss'', lr:r)
        match' (t:_)            (s:ss)         = error ("Can't match declared " ++ show t ++
                                                        " and actual " ++ show s)


