{
module Main(main) where

import qualified Tokens

import ParserMonad
import Control.Monad(liftM, liftM2)
import qualified Type

}

%name      parseSTAR star
%tokentype { Tokens.Token }
%monad     { Parser       } { parseThen  } { parseReturn }
%lexer     { getToken     } { Tokens.EOF }
%error     { parseError   }

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
      | Save list1(entry) Endsave {% savedEntry (Tokens.tokenValue $1) $2 }

-- should there be flatEntry and entry (with ref allowed or not)
flatData :: { Type.STAREntry }
flatEntry : item    { $1 }
	  | topLoop { $1 }

item :: { Type.STAREntry }
item : Name value { $2 (Tokens.tokenValue $1) }

value :: { Type.STARKey -> Type.STAREntry }
value : Text { \k -> Entry k (Tokens.tokenValue $1) }
      | Ref  {% deref (Tokens.tokenValue $1) >>= \f -> case f of Frame _ es -> return (\k -> Frame k es) }

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

globalSTARKey :: STARKey
globalSTARKey = ""

runParse :: String -> Either String [Type.STARBlock]
runParse input = case parseSTAR' (initState input) of
                   (st, ParseFail    s) -> let Tokens.AlexPn _ l c = extractPos . extractInput $ st
                                             in  Left $ ("Parse error " ++ s ++
                                                         " at line " ++ show l ++
                                                         " column " ++ show c)
                   (_ , ParseSuccess b) -> Right b
  where Parser parseSTAR' = parseSTAR

main = do r <- getContents
          print $ runParse r

}

