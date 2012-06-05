{
module Main(main) where
--module Parser (main) where

import Tokens
import ParserMonad
import Control.Monad(liftM2)

}

--%name     parseSTAR star
%name     parseSTAR valueListEntry
%tokentype { Token }
%monad     { Parser     } { parseThen } { parseReturn }
%lexer     { getToken   } { EOF }
%error     { parseError }

%token
    Name     { Name    n }
    Text     { Text    t }
    Save     { Save    s }
    Endsave  { EndSave   }
    Loop     { Loop      }
    EndLoop  { EndLoop   }
    Data     { Data    d }
    Global   { Global    }
    Ref      { Ref     n }
--  err      { Err       }
%%

list(a)  : a list(a) { $1:$2 }
         |           { []    }

list1(a) : a list(a) { $1:$2 }

star :: { STARDict }
star : list(block) { $1 }

block :: { (STARKey, STARDict) }
block : blockHeader blockContents { ($1, $2) }

blockHeader :: { STARKey }
blockHeader : Global { globalSTARKey }
	    | Data   { tokenValue $1 }

blockContents :: { STARDict }
blockContents :  list(item) { $1 }

item :: { (STARKey, STARValue) }
item : Name entry { (tokenValue $1, $2) }

entry :: { STARValue }
entry : Text                            {  VText $ tokenValue $1 }
      | Ref                             {% deref $ tokenValue $1 }
      | topLoop                         {  VList              $1 }

topLoop :: { [STARDict] }
topLoop : Loop nameList valueList { error "Unimplemented!" } 
topLoop : loop                    { error "Unimplemented!" } 

loop :: { [STARType] }
loop : Loop nameList EndLoop { $2 } 

nameList :: { [STARType] }
nameList : list1(nameListEntry) { $1 }

nameListEntry :: { STARType }
nameListEntry : Name { TSimple  $ tokenValue $1 }
	      | loop { TComplex $1 }

valueList :: { [STARStruct] }
valueList : list1(valueListEntry) { $1 }

valueListEntry :: { STARStruct }
valueListEntry : Text    { SText $ tokenValue $1 }
               | EndLoop { SStop                 }

{

deref x = return . VText $ "$" ++ x -- TODO: use attribute grammar?

parseError t = parseFail $ "parse error at token " ++ show t

type STARBlock  = (STARKey, STARDict)
data STARType   = TSimple  STARKey
                | TComplex [STARType]
  deriving (Show,Eq)
data STARStruct = SText String
                | SStop
  deriving (Show,Eq)

undefGetToken = undefined

globalSTARKey :: STARKey
globalSTARKey = ""

--runParse :: String -> Either String [STARBlock]
runParse input = case parseSTAR' (initState input) of
                   (st, ParseFail s) -> let AlexPn _ l c = extractPos . extractInput $ st
                                        in  Left $ ("Parse error " ++ s ++
                                                    " at line " ++ show l ++
                                                    " column " ++ show c)
                   (_ , ParseSuccess b) -> Right b
  where Parser parseSTAR' = parseSTAR

main = do r <- getContents
          print $ runParse r

}

