{
{-# LANGUAGE OverloadedStrings, BangPatterns, NoMonomorphismRestriction #-}
module Main(main) where

import qualified Tokens

import Control.Monad(liftM, liftM2)
import Control.Monad.State.Strict
import qualified Type
import Prelude hiding (String, getContents, drop, take, (++))
import Data.ByteString.Char8    as BSC
import Data.ByteString.Internal as BSI
import Control.DeepSeq
import Control.Exception(assert)
import qualified GHC.Exts as Happy_GHC_Exts

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
valueListEntry : Text     {% liftM (\p -> let t = Tokens.tokenValue $1 in t `seq` p `seq` SText p  t) Tokens.getPos }
               | Ref      {% liftM (\p -> let t = Tokens.tokenValue $1 in t `seq` p `seq` SRef  p  t) Tokens.getPos }
               | EndLoop  {% liftM SStop                                                              Tokens.getPos }
               | semilist {% liftM (\p ->                                $1 `seq` p `seq` SText p $1) Tokens.getPos }
{

cheatConcat (PS x1 s1 l1) (PS x2 s2 l2) = assert (x1 == x2) $ PS x1 s1 (s2+l2-s1)

(++) = BSC.append

data STARType   = TSimple  Type.STARKey
                | TComplex [STARType]
  deriving (Show, Eq)

data STARStruct = SText Tokens.AlexPosn Type.String -- keep position for matchTypesValues error reporting!
                | SRef  Tokens.AlexPosn Type.String -- TODO: implement!!!
                | SStop Tokens.AlexPosn
  deriving (Show,Eq)

matchTypesValues  :: [STARType] -> [STARStruct] -> Type.STAREntry
matchTypesValues !ts !ss = r
  where ([], r)          = matchTypesValues' ts ss

-- TODO: change to monad!
matchTypesValues' :: [STARType] -> [STARStruct] -> ([STARStruct], Type.STAREntry)
matchTypesValues' ts ss = r `deepseq` (ss', Type.Loop r)
  where (ss', r ) = match' ts ss
        match' :: [STARType] -> [STARStruct] -> ([STARStruct], [Type.STAREntry])
        match' []               (SStop p  :ss) = (ss, [])
        match' []               ss             = match' ts ss
        match' (TSimple  t :ts) (SText p s:ss) = let (ss',                r) = match'    ts ss
                                                 in  (ss', Type.Entry t s:r)
        match' (TSimple  t :ts) (SRef  p s:ss) = let (ss',                r) = match'    ts ss
                                                 in  (ss', Type.Ref   t s:r)
        match' (TComplex tc:ts) []             = error $ Prelude.concat ["Cannot find any more values to match ", show (TComplex tc)]
        match' (TComplex tc:ts) ss             = let (ss' , lr  ) = matchTypesValues' tc ss
                                                     (ss'',    r) = match'            ts ss'
                                                 in  (ss'', lr:r)
        match' (t:_)            (s:ss)         = error $ Prelude.concat ["Can't match declared ",
                                                                         show t,
                                                                         " and actual ",
                                                                         show s]

failToken tok = Tokens.parseError . BSC.concat $ ["parse error on ", BSC.pack $ show tok]

main = do r <- BSC.getContents
          case Tokens.runParser parseSTAR r of
            Left  (Tokens.ParseError l c st s) -> Prelude.putStrLn $ Prelude.concat ["Parse error in line ", show l,
                                                                                     " column ", show c,
                                                                                     ":", BSC.unpack s,
                                                                                     "(lexer state is ", show st, ")"]
            Right result                       -> print result

}

