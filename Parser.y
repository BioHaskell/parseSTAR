{
module Parser (main) where

import Tokens

}

--%name     parseSTAR star
%name     parseSTAR valueListEntry
%tokentype { Token }
%monad     { Parser }
%lexer     { alexScan } { AlexEOF }

%token
    Name     { Name }
    Text     { Text }
    Save     { Save }
    Endsave  { EndSave }
    Loop     { Loop }
    EndLoop  { EndLoop }
    Data     { Data }
    Global   { Global }
    Ref      { Ref }
--  err      { Err }
%%

--list(a) : a list(a) { $1:$2 }
--        |           { []    }

--list1(a) : a list(a) { $1:$2 }

--star :: { STARDict }
--star : list(block) { return $1 }
--
--block :: { (STARKey, STARDict) }
--block : blockHeader blockContents { return $ ($1, VList $2) }
--
--blockHeader :: { STARKey }
--blockHeader : Global { return globalSTARKey }
--	    | Data   { return $1            }
--
--blockContents :: { STARDict }
--blockContents :  list(item) { return $ $1 }
--
--item :: { (STARKey, STARValue) }
--item : Name entry { return ($1, $2) }
--
--entry :: { STARValue }
--entry : Text                            { return           $1 }
--      | Ref                             { return . deref $ $1 }
--      | topLoop                         { return           $1 }
--
--topLoop :: { [STARValue] }
--topLoop : Loop nameList valueList { error "Unimplemented!" } 
--topLoop : loop                    { error "Unimplemented!" } 
--
--loop :: { [STARType] }
--loop : Loop nameList EndLoop { error "Unimplemented!" } 
--
--nameList :: { STARType }
--nameList : list1(nameListEntry) { return $1 }
--
--nameListEntry :: { STARType }
--nameListEntry : Name { return $ TSimple  $1 }
--	      | loop { return $ TComplex $1 }
--
--valueList :: { [STARStruct] }
--valueList : list1(valueListEntry) { return $1 }

valueListEntry :: { STARStruct }
valueListEntry : Text    { case $1 of Text _ a -> SText a }
               | EndLoop { SStop    }

{

-- TODO: split parser monad?
data ParseResult a = ParseSuccess AlexPosn a
                   | ParseFail    AlexPosn String
  deriving (Show,Eq)

newtype Parser a = Parser (String -> AlexPosn -> ParseResult a)

getPos = Parser (\s p -> ParseSuccess p p)

deref x = "$" ++ x -- TODO: use attribute grammar!

parseThen :: Parser a -> (a -> Parser b) -> Parser b
(Parser a) `parseThen` pb = Parser (\s l -> case a s l of
                                      ParseFail    l s@pfail -> ParseFail l s
                                      ParseSuccess l a       -> case pb a of
                                                                  Parser f -> f s l)

parseFail s = Parser (\_ p -> ParseFail p s)

parseReturn :: a -> Parser a
parseReturn a = Parser (\s l -> ParseSuccess l a)

instance Monad Parser where
  (>>=)  = parseThen
  return = parseReturn
  fail   = parseFail

happyError = parseFail "Happy Error!!!"

type STARKey    = String
data STARValue  = VText String | VList [STARDict]
  deriving (Show,Eq)
type STARDict   = [(STARKey, STARValue)]
type STARBlock  = (STARKey, STARDict)
data STARType   = TSimple  STARKey
                | TComplex [STARType]
  deriving (Show,Eq)
data STARStruct = SText String
                | SStop
  deriving (Show,Eq)

globalSTARKey = ""

--runParse :: String -> Either String [STARBlock]
runParse input = case parseSTAR' input startLoc of
                   ParseFail    l s -> Left $ "Parse error " ++ s ++ " at " ++ show l
                   ParseSuccess l b -> Right b
  where Parser parseSTAR' = parseSTAR
        startLoc = AlexPn 0 0 0

main = do r <- getContents
          print $ runParse r

}

