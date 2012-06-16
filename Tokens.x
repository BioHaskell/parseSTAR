{
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Tokens (Token(..), ParserT(..), unParserT,
               tokenValue,
               alexScan,
               AlexPosn(..), AlexReturn(..), AlexInput(..),
               initState,
               parseThen, parseReturn, parseError, ParserM(..), runParserT, runParser,
               getPos, getToken,
               ParseError(..)
              ) where

import Prelude hiding(String, take, drop)
import Type(String)
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
import Data.ByteString.Internal(ByteString(..))
import Control.Exception(assert)

-- Question: case insensitive?
-- NOTE: disallowed comments '!' '#' within strings (including ";")
}

%wrapper "posn-bytestring-strict"

$white       = [ \t \n \f \v \r \  ]
$nonwhite    = ~ $white # [ \# \! ] -- non-whitespace
$nonspecial  = $nonwhite # ['"\;\$] -- not whitespace and not underline
$nonspec     = [a-zA-Z0-9_] -- not whitespace and not underline
$nonunder    = $nonspecial # \_ -- not whitespace and not underline
$commentChar = [\!\#]
$under       = [\_]
$dollar      = \$ 
$eoln        = \n
$semi        = \;
$noneoln     = [^\n]
$noneolnsemi = [^\n\;]
$singleQuote = [\']
$doubleQuote = [\"]
$nonsemi     = [^\;]

tokens :-
<0>  $white +					       ;
<0>  $commentChar .* $eoln                             ;
<0>  $under $nonspecial*   / $white                    { (\p s -> Name . drop (BSC.length "_"    )   $ s , 0         ) } 
<0>  "save_" $nonspecial+                              { (\p s -> Save . drop (BSC.length "save_")   $ s , 0         ) }
<0>  "save_"   / $white                                { (\p s -> EndSave                                , 0         ) }
<0>  "stop_"                                           { (\p s -> EndLoop                                , 0         ) }
<0>  "loop_"                                           { (\p s -> Loop                                   , 0         ) }
<0>  "global_"                                         { (\p s -> Global                                 , 0         ) }
<0>  "data_" $nonwhite+ / $white                       { (\p s -> Data . chopFront "data_"           $ s , 0         ) }
<0>  $dollar $nonwhite+ / $white                       { (\p s -> Ref  . chopFront "$"               $ s , 0         ) }
<0>  $singleQuote [^\n\']+ $singleQuote                { (\p s -> Text . chop "\'"                   $ s , 0         ) }
<0>  $doubleQuote [^\n\"]+ $doubleQuote                { (\p s -> Text . chop "\""                   $ s , 0         ) }
<0>  $nonunder $nonspecial* / $white                   { (\p s -> Text s                                 , 0         ) }
<0>  ^$semi $eoln                                      { (\p s -> SemiStart (stringStep s   2 )          , semistring) }
<semistring> ^$semi                                    { (\p s -> SemiEnd   (stringStep s (-2))          , 0         ) }
<semistring> ^[^\;] .* $eoln                           ;
<semistring> ^ $eoln                                   ;
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

parseError msg = do ((AlexPn l c _, _, _), state) <- get
                    throwError $ ParseError l c state msg

parseReturn = return

instance Error ParseError
  where
    strMsg msg = ParseError (-1) (-1) (-1) (BSC.pack msg)

chop :: String -> String -> String
chop s = chopFront s . chopTail s

chopFront :: String -> String -> String
chopFront s t | BSC.length s > BSC.length t                                     = error $ "Cannot chop " ++ show s ++ " from " ++ show t ++ "!"
chopFront s t | (chopped, result) <- BSC.splitAt (BSC.length s) t               = if chopped == s
                                                                                    then result
                                                                                    else error ("Cannot chop " ++ show s ++
                                                                                                " when prefix is " ++ show chopped ++ "!")

chopTail :: String -> String -> String
chopTail s t | BSC.length s > BSC.length t                                      = error $ "Cannot chop " ++ show s ++ " from " ++ show t ++ "!"
chopTail s t | (result, chopped) <- BSC.splitAt (BSC.length t - BSC.length s) t = if chopped == s
                                                                                    then result
                                                                                    else error $ "Cannot chop " ++ show s ++ " when suffix is " ++ show chopped ++ "!"


{-
chopS :: String -> String -> String
chopS s t              = chopS' s s t

chopS' :: String -> String -> String -> String
chopS' ""     s cs     = tailCutS s cs
chopS' bbs s ccs | Just (b, bs) <- BSC.uncons bbs, Just (c, cs) <- BSC.uncons ccs = if b==c then chopS' bs s cs
                                                                                            else error $ "Cannot chop: " ++ show b ++ " <> " ++ show c
chopS' bs  s cs                                                                   = error $ "Cannot chop: " ++ show bs ++ " from " ++ show cs

chop c ds | BSC.head ds == c = tailCut c $ BSC.tail ds
chop _ ""                    = error "String to short to chop!"
chop c s                     = error $ "Cannot chop boundary characters " ++ show c ++ " of " ++ show s ++ "!"

tailCutS :: String -> String -> String
tailCutS bs cs | BSC.length bs <= BSC.length cs + 1 = BSC.cons c             (tailCutS bs cs)
tailCutS bs cs | BSC.head bs == BSC.head cs         = tailCutS (BSC.tail bs) (BSC.tail cs)
tailCutS ""     ""                                  = ""
tailCutS bs     cs                                  = error $ "tailCutS " ++ show bs ++ " vs " ++ show cs
tailCut c d   | c == BSC.head d && BSC.length d == 1  = ""
tailCut c bbs | Just (b, bs) <- bbs                   = BSC.cons b (tailCut c bs) -- TODO: make efficient
-}

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

tokenValue (Name      s) = s
tokenValue (Text      s) = s
tokenValue (Comment   s) = s
tokenValue (Save      s) = s
tokenValue (Data      s) = s
tokenValue (Ref       s) = s
tokenValue (SemiEnd   s) = s
tokenValue (SemiStart s) = s
tokenValue _             = error "Wrong token"

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
                                        AlexError i                                    -> (Err "lexical error", (alexInput, alexState))
                                        AlexSkip  !newAlexInput len                    -> scanForToken (newAlexInput, alexState)
                                        AlexToken !newAlexInput toklen (act, newState) -> let (pos, _, str) = alexInput
                                                                                              tokStr        = BSC.take (fromIntegral toklen) str
                                                                                              !token        = act pos tokStr
                                                                                          in (token, (newAlexInput, newState))

stringStep (PS x s l) i = assert (si >  0) $ PS x si l
  where si = s+i

}
