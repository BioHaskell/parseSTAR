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

star :: { STARDict }
star : list1(block) { STARDict $1 }

block :: { (Maybe STARKey, STARDict) }
block : globalBlock { $1 }
      | dataBlock   { $1 }

globalBlock :: { Type.STARBlock }
globalBlock : Global list1(flatData) { Type.Global $2 }

dataBlock :: { Type.STARBlock }
dataBlock : Data list1(entry) { Type.Data (Tokens.tokenValue $1) $2 }

entry :: { Type.STAREntry }
entry : flatData                  { $1 }
      | Save list1(entry) Endsave {% savedEntry (Tokens.tokenValue $1) $2 }

-- should there be flatEntry and entry (with ref allowed or not)
flatData :: { Type.STAREntry }
flatEntry : Name value { Entry (Tokens.tokenValue $1) $2 }
	  | loop       { Loop $1                  }

value :: { Type.STAREntry }
value : Text {  Tokens.tokenValue $1 }
      | Ref  {% deref $ Tokens.tokenValue $1 }

topLoop :: { Type.STARDict }
topLoop : Loop nameList valueList { matchTypesValues $2 $3 }

loop :: { [STARType] }
loop : Loop nameList EndLoop    { $2 }

nameList :: { [STARType] }
nameList : list1(nameListEntry) { $1 }

nameListEntry :: { STARType }
nameListEntry : Name { TSimple  $ Tokens.tokenValue $1 }
	      | loop { TComplex $1 }

valueList :: { [STARStruct] }
valueList : list1(valueListEntry) { $1 }

valueListEntry :: { STARStruct }
valueListEntry : Text    {% liftM (\p -> SText p $ Tokens.tokenValue $1) getPos }
               | EndLoop {% liftM SStop getPos                           }

{

parseError t = parseFail $ "parse error at token " ++ show t

data STARType   = TSimple  STARKey
                | TComplex [STARType]
  deriving (Show,Eq)
data STARStruct = SText Tokens.AlexPosn String -- keep position for matchTypesValues error reporting!
                | SStop Tokens.AlexPosn
  deriving (Show,Eq)

matchTypesValues :: [STARType] -> [STARStruct] -> STARDict
matchTypesValues = undefined

globalSTARKey :: STARKey
globalSTARKey = ""

runParse :: String -> Either String STARDict
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

