{
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, BangPatterns #-}
module Data.STAR.Tokens (Token(..), ParserT(..), unParserT,
                         tokenValue,
                         alexScan,
                         AlexPosn(..), AlexReturn(..), AlexInput(..),
                         initState,
                         parseThen, parseReturn, parseError, ParserM(..), runParserT, runParser,
                         getPos, getToken,
                         ParseError(..)
                        ) where

import Prelude hiding(String, take, drop)
import Data.STAR.Type(String)
import Data.ByteString.Char8 as BSC
import Data.ByteString.Char8(take,drop)

--import Text.Show.ByteString
import Control.Monad.State.Strict
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Identity
--import Data.ByteString.Char8 as BSC
import Data.Int
import Data.Binary.Put
import Data.Function
import Control.Exception(assert)
import Data.STAR.StringUtil(stringStep)

-- Question: case insensitive?
-- NOTE: disallowed comments '!' '#' within strings (including ";")
}

%wrapper "posn-bytestring-strict"

$white         = [ \t \n \f \v \r \  ]
$nonwhiteFirst = ~ $white # [ \# \! ] -- non-whitespace
$nonwhite      = [ . \xfeff ] # $white -- non-whitespace
$nonspecial    = $nonwhite # ['"\;\$] -- not whitespace and not underline
$nonspec       = [a-zA-Z0-9_] -- not whitespace and not underline
$nonunder      = $nonwhiteFirst # ['"\;\$\_] -- not whitespace and not underline
$commentChar   = [\!\#]
$under         = [\_]
$dollar        = \$ 
$eoln          = \n
$semi          = \;
$noneoln       = [^\n]
$noneolnsemi   = [^\n\;]
$singleQuote   = [\']
$doubleQuote   = [\"]
$nonsemi       = [^\;]
$nonsingle     = ~ [\']
$nondouble     = ~ [\"]

tokens :-
<0>          $white +                                                        ;
<0>          $commentChar .* $eoln                                           ;
<0>          $under $nonspecial*   / $white                                  { (\p s -> Name . drop (BSC.length "_"    )   $ s , 0           ) } 
<0>          "save_" $nonwhite+                                              { (\p s -> Save . drop (BSC.length "save_")   $ s , 0           ) }
<0>          "save_"   / $white                                              { (\p s -> EndSave                                , 0           ) }
<0>          "stop_"                                                         { (\p s -> EndLoop                                , 0           ) }
<0>          "loop_"                                                         { (\p s -> Loop                                   , 0           ) }
<0>          "global_"                                                       { (\p s -> Global                                 , 0           ) }
<0>          "data_" $nonwhite+ / $white                                     { (\p s -> chopFront "data_" s Data               , 0           ) }
<0>          $dollar $nonwhite+ / $white                                     { (\p s -> chopFront "$"     s Ref                , 0           ) }
<0>          $nonunder $nonwhite* / $white                                   { (\p s -> Text s                                 , 0           ) }

<0>          $white ^ $singleQuote                                           { (\p s -> SemiStart $ stringStep s 1             , squotstring ) }
<0>          $white ^ $doubleQuote                                           { (\p s -> SemiStart $ stringStep s 1             , dquotstring ) }

<0>          ^$semi $eoln                                                    { (\p s -> SemiStart $ stringStep s   2           , semistring  ) }
<semistring> ^$semi                                                          { (\p s -> SemiEnd   $ stringStep s (-2)          , 0           ) }
<semistring> ^[^\;] .* $eoln                                                 ;
<semistring> ^ $eoln                                                         ;

<squotstring> $singleQuote / $nonwhite                                       ;
<squotstring> $nonsingle                                                     ;
<squotstring> $singleQuote / $white                                          { (\p s -> SemiEnd   $ stringStep s (-1)          , 0           ) }

<dquotstring> $doubleQuote / $nonwhite                                       ;
<dquotstring> $nondouble                                                     ;
<dquotstring> $doubleQuote / $white                                          { (\p s -> SemiEnd   $ stringStep s (-1)          , 0           ) }
{

data ParseError = ParseError Int Int Int String

type ParserT m a = ErrorT ParseError (StateT (AlexInput, Int) m) a

type ParserM a = ParserT Identity a

runParserT p s = runStateT (runErrorT p) s

runParserM p s = runIdentity $ runParserT p s

runParser parser input = case parsed of
                           (a, _endstate) -> a
  where
    parsed = runParserM parser (initState input)

parseThen = (>>=)

unParserT = id

parseError msg = do ((AlexPn byte line column, _, _), state) <- get
                    throwError $ ParseError line column state msg

parseReturn = return

instance Error ParseError
  where
    strMsg msg = ParseError (-1) (-1) (-1) (BSC.pack msg)

chop :: String -> String -> (String -> Token) -> Token
chop s arg cont = chopFront s arg $ \a -> chopTail s a cont 

bshow = BSC.pack . show

chopFront :: String -> String -> (String -> Token) -> Token
chopFront s t cont | BSC.length s > BSC.length t                       = Err $ BSC.concat ["Cannot chop ", bshow s, " from ", bshow t, "!"]
chopFront s t cont | (chopped, result) <- BSC.splitAt (BSC.length s) t = if chopped == s
                                                                           then cont result
                                                                           else Err $ BSC.concat ["Cannot chop ",      bshow s,
                                                                                                  " when prefix is ", bshow chopped, "!"]

chopTail :: String -> String -> (String -> Token) -> Token
chopTail s t cont | BSC.length s > BSC.length t                                      = Err $ BSC.concat ["Cannot chop ", bshow s, " from ", bshow t, "!"]
chopTail s t cont | (result, chopped) <- BSC.splitAt (BSC.length t - BSC.length s) t = if chopped == s
                                                                                         then cont result
                                                                                         else Err $ BSC.concat ["Cannot chop ",     bshow s,
                                                                                                                " when suffix is ", bshow chopped, "!"]


-- The token type:
data Token =
        White             |
        Name      !String |
        Text      !String |
        Comment   !String |
        Save      !String |
        EndSave           |
        Loop              |
        EndLoop           |
        Data      !String |
        Global            |
        Ref       !String |
        EOF               |
        SemiStart !String |
        SemiEnd   !String |
        Err       !String
  deriving (Eq,Show)

tokenValue (Name       s) = s
tokenValue (Text       s) = s
tokenValue (Comment    s) = s
tokenValue (Save       s) = s
tokenValue (Data       s) = s
tokenValue (Ref        s) = s
tokenValue (SemiEnd    s) = s
tokenValue (SemiStart  s) = s
tokenValue _              = error "Wrong token"

initState input = ((alexStartPos, '\n', input), 0)

firstLine  = BSC.takeWhile (/= '\n')
--firstLines s = intersperse "\n" . take 2 . splitWith '\n' $ s

getPos = do ((pos, _, _), _) <- get
            return pos

tokenTaker = lift $ mapState getToken' (return ())

getToken cont = do t <- tokenTaker
                   case t of
                     Err msg -> parseError "lexical error"
                     _       -> cont t

getToken' ((), alexInputState) = scanForToken alexInputState

scanForToken (alexInput, alexState) = case alexScan alexInput alexState of
                                        AlexEOF                                        -> (EOF, (alexInput, alexState))
                                        AlexError i                                    -> (Err "lexer error", (alexInput, alexState))
                                        AlexSkip  !newAlexInput len                    -> scanForToken (newAlexInput, alexState)
                                        AlexToken !newAlexInput toklen (act, newState) -> let (pos, _, str) = alexInput
                                                                                              tokStr        = BSC.take (fromIntegral toklen) str
                                                                                              !token        = act pos tokStr
                                                                                          in (token, (newAlexInput, newState))

}
