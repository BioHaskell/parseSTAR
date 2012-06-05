{
module Main(main) where
--module Parser (main) where

import Tokens
import ParserMonad
import Control.Monad(liftM, liftM2)

}

%name     parseSTAR star
%tokentype { Token }
%monad     { Parser     } { parseThen } { parseReturn }
%lexer     { getToken   } { EOF }
%error     { parseError }

%token
    Name     { Name    _ }
    Text     { Text    _ }
    Save     { Save    _ }
    Endsave  { EndSave   }
    Loop     { Loop      }
    EndLoop  { EndLoop   }
    Data     { Data    _ }
    Global   { Global    }
    Ref      { Ref     _ }
--  err      { Err       }
%%

list(a)  : a list(a) { $1:$2 }
         |           { []    }

list1(a) : a list(a) { $1:$2 }

star :: { STARDict }
star : list1(block) { mkSTARDict $1 }

block :: { (STARKey, STARValue) }
block : blockHeader blockContents { ($1, VList $2) }

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

topLoop :: { STARDict }
topLoop : Loop nameList valueList { matchTypesValues $2 $3 }
--topLoop : loop                    { $1 }

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
valueListEntry : Text    {% liftM (\p -> SText p $ tokenValue $1) getPos } 
               | EndLoop {% liftM SStop getPos                           }

{

deref x = return . VText $ "$" ++ x -- TODO: use attribute grammar?

parseError t = parseFail $ "parse error at token " ++ show t

data STARType   = TSimple  STARKey
                | TComplex [STARType]
  deriving (Show,Eq)
data STARStruct = SText AlexPosn String -- keep position for matchTypesValues error reporting!
                | SStop AlexPosn
  deriving (Show,Eq)

mkSTARDict :: [(STARKey, STARValue)] -> STARDict
mkSTARDict = id	   

matchTypesValues :: [STARType] -> [STARStruct] -> STARDict
matchTypesValues = undefined

globalSTARKey :: STARKey
globalSTARKey = ""

runParse :: String -> Either String STARDict
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

