{
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Tokens (Token(..), ParserT(..), unParserT,
               tokenValue,
               alexScan, alexScanTokens,
               AlexPosn(..), AlexReturn(..), AlexInput(..),
               initState,
               parseThen, parseReturn, parseError, ParserM(..), runParserT, runParser,
               getPos, getToken,
               ParseError(..)
              ) where

import Prelude hiding(String, take, drop)
import Type(String)
import Data.ByteString.Lazy.Char8 as BSC
import Data.ByteString.Lazy.Char8(take,drop)

--import Text.Show.ByteString
import Control.Monad.State.Strict
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Identity
--import Data.ByteString.Char8 as BSC
import Data.Int
import Data.Binary.Put
import Data.Function

-- Question: case insensitive?
-- NOTE: disallowed comments '!' '#' within strings (including ";")
}

%wrapper "posn-bytestring"

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
  $white +					    ;
  $commentChar .* $eoln                                    ;
  $under $nonspecial*   / $white                    { (\p s -> Name . drop (BSC.length "_"    )   $ s ) }  
  "save_" $nonspecial+                              { (\p s -> Save . drop (BSC.length "save_")   $ s ) }
  "save_"   / $white                                { (\p s -> EndSave                                ) }
  "stop_"                                           { (\p s -> EndLoop                                ) }
  "loop_"                                           { (\p s -> Loop                                   ) }
  "global_"                                         { (\p s -> Global                                 ) }
  "data_" $nonwhite+ / $white                       { (\p s -> Data . chopFront "data_"           $ s ) }
  $dollar $nonwhite+ / $white                       { (\p s -> Ref  . chopFront "$"               $ s ) }
  $singleQuote $noneoln+ $singleQuote               { (\p s -> Text . chop "\'"                   $ s ) }
  $doubleQuote $noneoln+ $doubleQuote               { (\p s -> Text . chop "\""                   $ s ) }
  $nonunder $nonspecial* / $white                   { (\p s -> Text s                                 ) }
  ^$semi $eoln [.\n]* $semi $eoln                   { (\p s -> Text . chop ";\n"                  $ s ) }
  --^";\n" $nonsemi* $eoln ";\n"                    { (\p s -> Text . chop ";\n"                  $ s ) }
  --^";\n" ( $nonsemi $noneolnsemi* $eoln )* ";\n"  { (\p s -> Text . chop ";\n"                  $ s ) }

{

data ParseError = ParseError Int Int String

newtype ParserT m a = ParserT (ErrorT ParseError (StateT AlexInput m) a)

type ParserM = ParserT Identity

runParserT (ParserT p) s = runStateT (runErrorT p) s

runParserM p s = runIdentity $ runParserT p s

runParser parser input = case parsed of
                           (a, _endstate) -> a
  where
    parsed = Tokens.runParserM parser (initState input)

parseError msg = ParserT (do (AlexPn l c _, _, _) <- get
                             throwError $ ParseError l c msg)

unParserT (ParserT f) = f

a `parseThen` b = ParserT (do x <- unParserT a
                              unParserT $ b x)
                                        

parseReturn a = ParserT (return a)

instance Error ParseError
  where
    strMsg msg = ParseError (-1) (-1) (BSC.pack msg)

instance MonadTrans ParserT
  where
    lift f = ParserT (lift . lift $ f)

instance (Monad m)=>Monad (ParserT m)
  where
    (>>=)  = parseThen
    return = parseReturn
    fail   = parseError . BSC.pack
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
        White           |
        Name    String  |
        Text    String  |
        Comment String  |
        Save    String  |
        EndSave         |
        Loop            |
        EndLoop         |
        Data    String  |
        Global          |
        Ref     String  |
        EOF             |
        Err     String
  deriving (Eq,Show)

tokenValue (Name    s) = s
tokenValue (Text    s) = s
tokenValue (Comment s) = s
tokenValue (Save    s) = s
tokenValue (Data    s) = s
tokenValue (Ref     s) = s
tokenValue _             = error "Wrong token"

initState input = (AlexPn 0 0 0, '\n', input)

firstLine  = BSC.takeWhile (/= '\n')
--firstLines s = intersperse "\n" . take 2 . splitWith '\n' $ s

getPos = Tokens.ParserT (do (pos, _, _) <- get
                            return pos)

tokenTaker = Tokens.ParserT $ lift $ mapState getToken' (return ())

getToken cont = do t <- tokenTaker
                   case t of
                     Tokens.Err msg -> fail "lexical error"
                     _              -> cont t

getToken' ((), alexInput) = scanForToken alexInput

scanForToken alexState = case Tokens.alexScan alexState 0 of
                                    Tokens.AlexEOF                            -> (Tokens.EOF, alexState)
                                    Tokens.AlexError i                        -> (Tokens.Err "lexical error", alexState)
                                    Tokens.AlexSkip  !newAlexState len        -> scanForToken newAlexState
                                    Tokens.AlexToken !newAlexState toklen act -> let (pos, _, str) = alexState
                                                                                     tokStr        = BSC.take (fromIntegral toklen) str
                                                                                     !token        = act pos tokStr
                                                                                 in (token, newAlexState)

}
