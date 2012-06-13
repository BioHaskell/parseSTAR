{
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main(main) where

import qualified Tokens

import Control.Monad(liftM, liftM2)
import Control.Monad.State
import qualified Type
import Prelude hiding (String, getContents, drop, take, (++))
import Data.ByteString.Lazy.Char8 as BSC

}

%name      parseSTAR star
%tokentype { Tokens.Token     }
%monad     { Tokens.ParserT m } 
%lexer     { getToken         } { Tokens.EOF }
%error     { failToken        }

%token
    Name     { Tokens.Name    _ }
    Text     { Tokens.Text    _ }
    Save     { Tokens.Save    _ }
    Endsave  { Tokens.EndSave   }
    Loop     { Tokens.Loop      }
    EndLoop  { Tokens.EndLoop   }
    Data     { Tokens.Data    _ }
    Global   { Tokens.Global    }
    Ref      { Tokens.Ref     _ }
--  Err      { Tokens.Err       }
%%

list(a)  : a list(a) { $1:$2 }
         |           { []    }

list1(a) : a list(a) { $1:$2 }

star :: { [Type.STARBlock] }
star : list1(block) { $1 }

block :: { Type.STARBlock }
block : Global list1(flatData) { Type.Global $2 }
      | Data   list1(entry   ) { Type.Data (Tokens.tokenValue $1) $2 }

entry :: { Type.STAREntry }
entry : flatData                  { $1 }
      | Save list1(entry) Endsave { Type.Frame (Tokens.tokenValue $1) $2 }

-- should there be flatEntry and entry (with ref allowed or not)
flatData :: { Type.STAREntry }
flatEntry : item    { $1 }
	  | topLoop { $1 }

item :: { Type.STAREntry }
item : Name value { $2 (Tokens.tokenValue $1) }

value :: { Type.STARKey -> Type.STAREntry }
value : Text { \k -> Type.Entry k (Tokens.tokenValue $1) }
      | Ref  { \k -> Type.Ref   k (Tokens.tokenValue $1) }
--{% deref (Tokens.tokenValue $1) >>= \f -> case f of Type.Frame _ es -> return (\k -> Frame k es) }

topLoop :: { Type.STAREntry }
topLoop : Loop list1(structure) list1(valueListEntry) { matchTypesValues $2 $3 }

structureList :: { [STARType] }
structureList : list1(structure)          { $1 }

structure :: { STARType }
structure : Name                          { TSimple  (Tokens.tokenValue $1) }
	  | Loop list1(structure) EndLoop { TComplex $2                     }

--nameList :: { [STARType] }
--nameList : list1(nameListEntry) { $1 }

--nameListEntry :: { STARType }
--nameListEntry : Name { TSimple  $ Tokens.tokenValue $1 }
--              | loop { TComplex $1 }

valueList :: { [STARStruct] }
valueList : list1(valueListEntry) { $1 }

valueListEntry :: { STARStruct }
valueListEntry : Text    {% liftM (\p -> SText p $ Tokens.tokenValue $1) getPos }
               | Ref     {% liftM (\p -> SRef  p $ Tokens.tokenValue $1) getPos }
               | EndLoop {% liftM SStop getPos                                  }

{

getPos = do (pos, _, _) <- get
            return pos

getToken :: (Tokens.Token -> Tokens.ParserT m a) -> Tokens.ParserT m a
getToken cont = do r <- lift (mapStateT getToken' (return ()))
                   cont r

--getToken' :: ((), Tokens.AlexInput) -> (Tokens.Token, Tokens.AlexInput)
getToken' ((), ((), alexState)) = case Tokens.alexScan alexState 0 of
                                    Tokens.AlexEOF                            -> return (Tokens.EOF, alexState)
                                    Tokens.AlexError i                        -> fail "lexical error" 
                                    Tokens.AlexSkip  !newAlexState len        -> getToken' ((), ((), newAlexState))
                                    Tokens.AlexToken !newAlexState toklen act -> let (pos, _, str) = alexState
                                                                                     tokStr        = BSC.take (fromIntegral toklen) str
                                                                                     !token        = act pos tokStr
                                                                                 in  return (token, newAlexState)

(++) = BSC.append

{-
runParse :: BSC.ByteString -> Either BSC.ByteString [Type.STARBlock]
runParse input = case parseSTAR' (initState input) of
                   (st, ParseFail    s) -> let Tokens.AlexPn _ l c = extractPos . extractInput $ st
                                             in  Left ("Parse error " ++ s ++
                                                       " at line " ++ BSC.pack (show l) ++
                                                       " column " ++ BSC.pack (show c))
                   (_ , ParseSuccess b) -> Right b
  where Parser parseSTAR' = parseSTAR
-}

data STARType   = TSimple  Type.STARKey
                | TComplex [STARType]
  deriving (Show,Eq)

data STARStruct = SText Tokens.AlexPosn Type.String -- keep position for matchTypesValues error reporting!
                | SRef  Tokens.AlexPosn Type.String -- TODO: implement!!!
                | SStop Tokens.AlexPosn
  deriving (Show,Eq)

matchTypesValues  :: [STARType] -> [STARStruct] -> Type.STAREntry
matchTypesValues ts ss = r
  where ([], r)        = matchTypesValues' ts ss

matchTypesValues' :: [STARType] -> [STARStruct] -> ([STARStruct], Type.STAREntry)
matchTypesValues' ts ss = (ss', Type.Loop r)
  where (ss', r ) = match' ts ss
        match' :: [STARType] -> [STARStruct] -> ([STARStruct], [Type.STAREntry])
        match' []               (SStop p  :ss) = (ss, [])
        match' []               ss             = match' ts ss
        match' (TSimple  t :ts) (SText p s:ss) = let (ss',                r) = match'    ts ss
                                                 in  (ss', Type.Entry t s:r)
        match' (TSimple  t :ts) (SRef  p s:ss) = let (ss',                r) = match'    ts ss
                                                 in  (ss', Type.Ref   t s:r)
        match' (TComplex tc:ts) []             = error . Prelude.concat ["Cannot find any more values to match ", show (TComplex tc)]
        match' (TComplex tc:ts) ss             = let (ss' , lr  ) = matchTypesValues' tc ss
                                                     (ss'',    r) = match'            ts ss'
                                                 in  (ss'', lr:r)
        match' (t:_)            (s:ss)         = error . Prelude.concat ["Can't match declared ",
                                                                         show t,
                                                                         " and actual ",
                                                                         show s]

failToken tok = fail $ Prelude.concat ["parse error on ", show tok]

{-
main = do r <- getContents
          print $ runParse r
-}

}

