{
{-# LANGUAGE OverloadedStrings #-}
module Tokens (Token(..),
               tokenValue,
               alexScan, alexScanTokens,
               AlexPosn(..), AlexReturn(..), AlexInput(..),
               alexStartPos
              ) where

import Prelude hiding(String, take, drop)
import Type(String)
import Data.ByteString.Lazy.Char8 as BSC
import Data.ByteString.Lazy.Char8(take,drop)


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
  $under $nonspecial*   / $white                    { (\p s -> Name . drop (BSC.length "_"    ) $ s ) }  
  "save_" $nonspecial+                              { (\p s -> Save . drop (BSC.length "save_") $ s ) }
  "save_"   / $white                                { (\p s -> EndSave                              ) }
  "stop_"                                           { (\p s -> EndLoop                              ) }
  "loop_"                                           { (\p s -> Loop                              ) }
  "global_"                                         { (\p s -> Global                              ) }
  "data_" $nonwhite+ / $white                       { (\p s -> Data . chopFront "data_"         $ s ) }
  $dollar $nonwhite+ / $white                       { (\p s -> Ref  . chopFront "$"             $ s ) }
  $singleQuote $noneoln+ $singleQuote               { (\p s -> Text . chop "\'"                 $ s ) }
  $doubleQuote $noneoln+ $doubleQuote               { (\p s -> Text . chop "\""                 $ s ) }
  $nonunder $nonspecial* / $white                   { (\p s -> Text s                           ) }
  ^$semi $eoln [.\n]* $semi $eoln                   { (\p s -> Text . chop ";\n"                  $ s ) }
  --^";\n" $nonsemi* $eoln ";\n"                    { (\p s -> Text . chop ";\n"                  $ s ) }
  --^";\n" ( $nonsemi $noneolnsemi* $eoln )* ";\n"  { (\p s -> Text . chop ";\n"                  $ s ) }

{

chop :: String -> String -> String
chop s = chopFront s . chopTail s

chopFront :: String -> String -> String
chopFront s t | BSC.length s > BSC.length t                                     = error $ "Cannot chop " ++ show s ++ " from " ++ show t ++ "!"
chopFront s t | (chopped, result) <- BSC.splitAt (BSC.length s) t               = if chopped == s
                                                                                    then result
                                                                                    else error $ "Cannot chop " ++ show s ++ " when prefix is " ++ show chopped ++ "!"

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
        Err     String AlexPosn String
  deriving (Eq,Show)

tokenValue (Name    s) = s
tokenValue (Text    s) = s
tokenValue (Comment s) = s
tokenValue (Save    s) = s
tokenValue (Data    s) = s
tokenValue (Ref     s) = s
tokenValue _             = error "Wrong token"

firstLine  = BSC.takeWhile (/= '\n')
--firstLines s = intersperse "\n" . take 2 . splitWith '\n' $ s

}
