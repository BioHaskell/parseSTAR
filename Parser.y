{
module Main(main) where
--module Parser (main) where

import Tokens
import ParserMonad

}

--%name     parseSTAR star
%name     parseSTAR valueListEntry
%tokentype { Token }
%monad     { Parser }
%lexer     { getToken } { EOF p }
%error     { parseError }

%token
    Name     { Name    p n }
    Text     { Text    p t }
    Save     { Save    p s }
    Endsave  { EndSave p   }
    Loop     { Loop    p   }
    EndLoop  { EndLoop p   }
    Data     { Data    p d }
    Global   { Global  p   }
    Ref      { Ref     p n }
--  err      { Err         }
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

deref x = "$" ++ x -- TODO: use attribute grammar?

parseError t = parseFail $ "parse error at token " ++ show t

type STARBlock  = (STARKey, STARDict)
data STARType   = TSimple  STARKey
                | TComplex [STARType]
  deriving (Show,Eq)
data STARStruct = SText String
                | SStop
  deriving (Show,Eq)

undefGetToken = undefined

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

