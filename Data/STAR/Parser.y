{
{-# LANGUAGE OverloadedStrings, BangPatterns, NoMonomorphismRestriction #-}
module Data.STAR.Parser(parse, parseFile) where

import qualified Data.STAR.Tokens as Tokens

import Control.Monad(liftM, liftM2)
import Control.Monad.State.Strict
import qualified Data.STAR.Type as Type
import Prelude hiding (String, getContents, drop, take, (++))
import Data.ByteString.Char8    as BSC
import Control.DeepSeq
import qualified GHC.Exts as Happy_GHC_Exts
import Data.STAR.StringUtil

}

%name      parseSTAR star
%tokentype { Tokens.Token    }
%monad     { Tokens.ParserM  } { Tokens.parseThen } { Tokens.parseReturn }
%lexer     { Tokens.getToken } { Tokens.EOF       }
%error     { failToken       }

%token
    Name      { Tokens.Name      _ }
    Text      { Tokens.Text      _ }
    Save      { Tokens.Save      _ }
    Endsave   { Tokens.EndSave     }
    Loop      { Tokens.Loop        }
    EndLoop   { Tokens.EndLoop     }
    Data      { Tokens.Data      _ }
    Global    { Tokens.Global      }
    Ref       { Tokens.Ref       _ }
    SemiStart { Tokens.SemiStart _ }
    SemiEnd   { Tokens.SemiEnd   _ }
--  Err       { Tokens.Err         }
%%

list_(a) : list_(a) a { $2:$1 }
         |            { []    }

list(a) : list_(a) { Prelude.reverse $1 }

list1(a) : a list(a) { $1 `seq` $1:$2 }

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
item : Name value { let t = Tokens.tokenValue $1 in t `seq` $2 (Tokens.tokenValue $1) }

value :: { Type.STARKey -> Type.STAREntry }
value : Text     { let t = Tokens.tokenValue $1 in t `seq` \k -> Type.Entry k t  }
      | Ref      { let t = Tokens.tokenValue $1 in t `seq` \k -> Type.Ref   k t  }
      | semilist {                                $1 `seq` \k -> Type.Entry k $1 }
--{% deref (Tokens.tokenValue $1) >>= \f -> case f of Type.Frame _ es -> return (\k -> Frame k es) }

semilist :: { Type.String }
semilist : SemiStart SemiEnd { cheatConcat (Tokens.tokenValue $1) (Tokens.tokenValue $2) }

topLoop :: { Type.STAREntry }
topLoop : Loop list1(structure) list1(valueListEntry) { matchTypesValues $2 $3 }

structureList :: { [STARType] }
structureList : list1(structure)          { $1 }

structure :: { STARType }
structure : Name                          { TSimple  (Tokens.tokenValue $1) }
	  | Loop list1(structure) EndLoop { TComplex $2                     }

valueList :: { [STARStruct] }
valueList : list1(valueListEntry) { $1 }

valueListEntry :: { STARStruct }
valueListEntry : Text     { SText $ Tokens.tokenValue $1 }
               | Ref      { SRef  $ Tokens.tokenValue $1 }
               | EndLoop  { SStop                        }
               | semilist { SText                     $1 }
{

data STARType   = TSimple  Type.STARKey
                | TComplex [STARType]
  deriving (Show, Eq)

data STARStruct = SText Type.String -- keep position for matchTypesValues error reporting!
                | SRef  Type.String -- TODO: implement!!!
                | SStop 
  deriving (Show,Eq)

matchTypesValues  :: [STARType] -> [STARStruct] -> Type.STAREntry
matchTypesValues !ts !ss = matchTypesValues' ts ts ss [] finish
  where
    finish entries [] = Type.Loop $ Prelude.reverse entries

matchTypesValues' :: [STARType] -> [STARType] -> [STARStruct] -> [Type.STAREntry] -> ([Type.STAREntry] -> [STARStruct] -> a) -> a
matchTypesValues' (TSimple t:ts)   tts (SText  s:ss) !acc !cont = matchTypesValues' ts  tts ss (Type.Entry t s:acc) cont
matchTypesValues' (TSimple t:ts)   tts (SRef   s:ss) !acc !cont = matchTypesValues' ts  tts ss (Type.Ref   t s:acc) cont
matchTypesValues' (TComplex tc:ts) tts ss            !acc !cont = matchTypesValues' tc  tc  ss []                   loopCont
  where
    loopCont es sn = matchTypesValues' ts tts sn (Type.Loop (Prelude.reverse es):acc) cont
matchTypesValues' []               tts (SStop   :ss) !acc !cont = cont acc ss
matchTypesValues' []               tts ss            !acc !cont = matchTypesValues' tts tts ss acc                  cont
matchTypesValues' (t:_)            _   (s:ss)        !acc !cont = error $ Prelude.concat ["Can't match declared ",
                                                                                          show t,
                                                                                          " and actual ",
                                                                                          show s]

failToken tok = Tokens.parseError . BSC.concat $ ["parse error on ", BSC.pack $ show tok]

parse = Tokens.runParser parseSTAR

parseFile fname = do r <- simpleRead fname
                     case parse r of
                       Left  (Tokens.ParseError l c st s) -> return $ Left $ Prelude.concat ["Parse error in line ", show l,
                                                                                             " column ", show c,
                                                                                             ":", BSC.unpack s,
                                                                                             "(lexer state is ", show st, ")"]
                                                               
                       Right result                       -> return $ Right result


}

