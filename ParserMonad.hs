module ParserMonad(Parser(..), ParseResult(..), ParserState(..),
                   getPos, getInput, setInput,
                   getToken,
                   STARKey(..), STARValue(..), STARDict(..)) where

import Tokens

data ParseResult a = ParseSuccess a
                   | ParseFail    AlexPosn String
  deriving (Show,Eq)

data ParserState = ParserState { curInput :: AlexInput,
                                 saved    :: [(STARKey, STARValue)]
                               }

newtype Parser a = Parser { unParser :: ParserState -> (ParserState, ParseResult a) }


parseFail s = do p <- getPos
                 Parser (\st -> (st, ParseFail p s))

getPos = do (p, _, _, _) <- getInput
            return p

getInput   = Parser (\pst -> case pst of
                               ParserState inp sav -> (ParserState inp sav, ParseSuccess inp))
setInput i = Parser (\pst -> case pst of
                               ParserState inp sav -> (ParserState i   sav, ParseSuccess () ))

--deref x = "$" ++ x -- TODO: use attribute grammar!

parseThen :: Parser a -> (a -> Parser b) -> Parser b
(Parser a) `parseThen` pb = Parser (\st -> case a st of
                                             (st', ParseFail    l s) -> (st', ParseFail l s)
                                             (st', ParseSuccess   a) -> unParser (pb a) st')


parseReturn :: a -> Parser a
parseReturn a = Parser (\st -> case st of
                          ParserState i s ->
                           (st, ParseSuccess a))

instance Monad Parser where
  (>>=)  = parseThen
  return = parseReturn
  fail   = parseFail

type STARKey    = String
data STARValue  = VText String | VList [STARDict]
  deriving (Show,Eq)
type STARDict   = [(STARKey, STARValue)]

globalSTARKey = ""

getToken = do i <- getInput
              case alexScan i 0 of
                AlexEOF              -> do p <- getPos
                                           return $ EOF p
                AlexError i          -> parseFail "Lexical error"
                AlexSkip  i' len     -> do setInput i'
                                           getToken
                AlexToken i' len act -> do setInput i'
                                           let (p, _, _, s) = i
                                           return $ act p (take len s)

