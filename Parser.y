{
module Parser (main) where

import Tokens

}

%name     parseSTAR star
%tokentype { Token }
%monad     { Parser }
%lexer     { alexScan } { AlexEOF }

%token
    Name    { Name }
    Text    { Text }
    Save    { Save }
    Endsave { EndSave }
    Loop    { Loop }
    EndLoop { EndLoop }
    Data    { Data }
    Global  { Global }
    Ref     { Ref }
--  err     { Err }
%%

list(a) : a list(a) { $1:$2 }
        |           { []    }

list1(a) : a list(a) { $1:$2 }
         | a         { [$1]  }
 
star :: { STARDict }
star : list(block) { return $1 }

block :: { (STARKey, STARValue) }
block : blockHeader blockContents { return $ ($1, $2) }

blockHeader :: { STARKey }
blockHeader : Global { return "" }
	    | Data   { return $1 }

blockContents :: { STARDict }
blockContents :  list(item) { return $1 }

item :: { (STARKey, STARValue) }
item : Name entry { return ($1, $2) }

entry :: { STARValue }
entry : Text                            { return $1 }
      | Ref                             { return . deref $ $1 }
      | topLoop                         { return $1 }

topLoop :: { [STARValue] }
topLoop : Loop nameList valueList { error "Unimplemented!" } 
topLoop : loop                    { error "Unimplemented!" } 

loop :: { [STARType] }
loop : Loop nameList EndLoop { error "Unimplemented!" } 

nameList :: { STARType }
nameList : list1(nameListEntry) { return }

nameListEntry :: { STARType }
nameListEntry : Name { return $ TSimple  }
	      | loop { return $ TComplex }

valueList :: { [STARStruct] }
valueList : list1(valueListEntry) { return }

valueListEntry :: { STARStruct }
valueListEntry : Text                          { return . SSimple }	       
               | list1(valueListEntry) EndLoop { return . SComplex }	       

{

-- TODO: split parser monad?
data ParseResult a = ParseSuccess a
                   | ParseFail    AlexPosn String

newtype Parser a = Parser (AlexPosn -> ParseResult a)

deref = id -- TODO: use attribute grammar!
parseThen :: Parser a -> (a -> Parser b) -> Parser b
(Parser a) `parseThen` (Parser b) = Parser (\p -> case a p of
                                                    ParseFail t s  -> ParseFail t s
                                                    ParseSuccess a -> case b a of Parser r -> r)

parseFail s = Parser (\p -> ParseFail p s)

parseReturn :: a -> Parser a
parseReturn a = Parser (\p -> ParseSuccess a)

instance Monad Parser where
  (>>=)  = parseThen
  return = parseReturn
  fail   = parseFail

happyError = parseFail

type STARKey    = String
data STARValue  = VText String | VList [STARDict]
type STARDict   = [(STARKey, STARValue)]
type STARBlock  = (STARKey, STARDict)
data STARType   = TSimple  STARKey
                | TComplex [STARType]
data STARStruct = SSimple  String
                | SComplex [STARStruct]

parse :: [Token] -> Either String [STARBlock]
parse tokens = case parseSTAR tokens of
                 ParseFail    t s -> Left $ "Parse error " ++ s ++ " at " ++ show t
                 ParseSuccess sb  -> Right sb

main = do r <- getContents
          let ts = alexScanTokens r
          print $ parse ts

}

