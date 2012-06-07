module ParserMonad(Parser(..), ParseResult(..), ParserState(..),
                   parseReturn, parseFail, parseThen,
                   getPos, getInput, setInput,
                   getToken, initState, extractPos, extractInput,
                   STARKey(..), STARValue(..), STARDict(..),
                  ) where

import Tokens
import Type

data ParseResult a = ParseSuccess a
                   | ParseFail    String
  deriving (Show,Eq)

data ParserState = ParserState { curInput :: AlexInput,
                                 saved    :: STARDict
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

--deref x = "$" ++ x -- TODO: use attribute grammar!

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

--type STARDict   = [(STARKey, STARValue)]

globalSTARKey = ""

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

