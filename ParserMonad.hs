module ParserMonad(Parser(..), ParseResult(..), ParserState(..),
                   parseReturn, parseFail, parseThen,
                   getPos, getInput, setInput,
                   getToken, initState, extractPos, extractInput,
                   STARKey(..), STAREntry(..),
                   STARType(..), STARStruct(..), parseError, 
                   matchTypesValues,
                   savedEntry, deref
                  ) where

import Tokens
import Type

data ParseResult a = ParseSuccess a
                   | ParseFail    String
  deriving (Show,Eq)

data ParserState = ParserState { curInput :: AlexInput,
                                 saved    :: [STAREntry]
                               }

initState input = ParserState (alexStartPos, '\n', [], input) []

newtype Parser a = Parser { unParser :: ParserState -> (ParserState, ParseResult a) }


parseFail s = do p <- getPos
                 Parser (\st -> (st, ParseFail s))

extractInput (ParserState ci s) = ci

extractPos :: AlexInput -> AlexPosn
extractPos (p, _, _, _) = p

getPos = do i <- getInput
            return $ extractPos i

getInput   = Parser (\pst -> case pst of
                               ParserState inp sav -> (ParserState inp sav, ParseSuccess inp))
setInput i = Parser (\pst -> case pst of
                               ParserState inp sav -> (ParserState i   sav, ParseSuccess () ))

savedEntry :: STARKey -> [STAREntry] -> Parser STAREntry
-- Add error checking after save!
savedEntry k es = Parser pp
  where
    pp (ParserState p saved) = (ParserState p (f:saved),
                                ParseSuccess f)
    f = Frame k es

getSaved :: Parser [STAREntry]
getSaved = Parser parser
  where
    parser pst@(ParserState _ sd) = (pst, ParseSuccess sd)

setSaved :: [STAREntry] -> Parser ()
setSaved sd = Parser parser
  where
    parser (ParserState inp sd) = (ParserState inp sd, ParseSuccess ())

frameLookup :: String -> [STAREntry] -> Maybe STAREntry
frameLookup k (f@(Frame l _):fs) | k == l = Just f
frameLookup k (_            :fs)          = frameLookup k fs
frameLookup k []                          = Nothing

deref :: STARKey -> Parser STAREntry
deref x = do sd <- getSaved
             case frameLookup x sd of
               Nothing -> fail ("Cannot locate frame " ++ show x)
               Just f  -> return f
--
parseThen :: Parser a -> (a -> Parser b) -> Parser b
(Parser a) `parseThen` pb = Parser (\st -> case a st of
                                             (st', ParseFail    s) -> (st', ParseFail s)
                                             (st', ParseSuccess a) -> unParser (pb a) st')

parseReturn :: a -> Parser a
parseReturn a = Parser (\st -> case st of
                          ParserState i s ->
                            (st, ParseSuccess a))

instance Monad Parser where
  (>>=)  = parseThen
  return = parseReturn
  fail   = parseFail

getToken :: (Token -> Parser a) -> Parser a
getToken cont = do i <- getInput
                   case alexScan i 0 of
                     AlexEOF              -> cont $ EOF
                     AlexError i          -> parseFail "Lexical error"
                     AlexSkip  i' len     -> do setInput i'
                                                getToken cont
                     AlexToken i' len act -> do setInput i'
                                                let (p, _, _, s) = i
                                                    tok          = act p (take len s)
                                                cont tok

parseError t = parseFail $ "parse error at token " ++ show t

data STARType   = TSimple  STARKey
                | TComplex [STARType]
  deriving (Show,Eq)
data STARStruct = SText Tokens.AlexPosn String -- keep position for matchTypesValues error reporting!
                | SRef  Tokens.AlexPosn String -- TODO: implement!!!
                | SStop Tokens.AlexPosn
  deriving (Show,Eq)

matchTypesValues  :: [STARType] -> [STARStruct] -> STAREntry
matchTypesValues ts ss = r
  where ([], r)        = matchTypesValues' ts ss

matchTypesValues' :: [STARType] -> [STARStruct] -> ([STARStruct], STAREntry)
matchTypesValues' ts ss = (ss', Type.Loop r)
  where ([] , r ) = match' ts ss
        (ss', r1) = matchTypesValues' ts ss
        match' :: [STARType] -> [STARStruct] -> ([STARStruct], [STAREntry])
        match' []               (SStop p:ss)   = (ss, [])
        match' []               ss             = match' ts ss
        match' (TSimple  t :ts) (SText p s:ss) = let (ss',           r) = match'    ts ss
                                                 in  (ss', Entry t s:r)
        match' (TComplex tc:ts) ss             = let (ss' , lr  ) = matchTypesValues' tc ss
                                                     (ss'',    r) = match'            ts ss'
                                                 in  (ss'', lr:r)

