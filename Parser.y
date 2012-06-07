{
module Main(main) where

import Tokens
import ParserMonad
import Control.Monad(liftM, liftM2)
--import STAR.Type
import Type

}

%name      parseSTAR star
%tokentype { Token      }
%monad     { Parser     } { parseThen } { parseReturn }
%lexer     { getToken   } { EOF       }
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

block :: { (Maybe STARKey, STARDict) }
block : globalBlock { $1 }
      | dataBlock   { $1 }

globalBlock :: { STARBlock }
globalBlock : Global list1(flatData) { Global $2 }

dataBlock :: { STARBlock }
dataBlock : Data list1(entry) { Data (Just . tokenValue $ $1) $2 }

entry :: { STAREntry }
entry : flatData                  { $1 }
      | Save list1(entry) Endsave {% undefined } --savedEntry (tokenValue $1, VList $2) }

-- should there be flatEntry and entry (with ref allowed or not)
flatData :: { (STARKey, STARValue) }
flatEntry : Name value { VEntry (tokenValue $1) $2 }
	  | loop       { VList  $1                 }

value :: { STARValue }
value : Text {  VText $ tokenValue $1 }
      | Ref  {% deref $ tokenValue $1 }

topLoop :: { STARDict }
topLoop : Loop nameList valueList { matchTypesValues $2 $3 }

loop :: { [STARType] }
loop : Loop nameList EndLoop    { $2 }

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

savedEntry :: (STARKey, STARValue) -> Parser (STARKey, STARValue)
-- Add error checking after save!
savedEntry e = Parser pp
  where
    pp (ParserState p saved) = (ParserState p (e:saved), ParseSuccess e)

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

