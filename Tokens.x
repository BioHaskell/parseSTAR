{
module Tokens (Token(..),
               tokenValue,
               alexScan, alexScanTokens,
               AlexPosn(..), AlexReturn(..), AlexInput(..),
               alexStartPos
              ) where
-- Question: case insensitive?
-- NOTE: disallowed comments '!' '#' within strings (including ";")
}

%wrapper "posn"

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
  $under $nonspecial*   / $white                    { (\p s -> Name . drop (length "_"    ) $ s ) }  
  "save_" $nonspecial+                              { (\p s -> Save . drop (length "save_") $ s ) }
  "save_"   / $white                                { (\p s -> EndSave                              ) }
  "stop_"                                           { (\p s -> EndLoop                              ) }
  "loop_"                                           { (\p s -> Loop                              ) }
  "global_"                                         { (\p s -> Global                              ) }
  "data_" $nonwhite+ / $white                       { (\p s -> Data . drop (length "data_") $ s ) }
  $dollar $nonwhite+ / $white                       { (\p s -> Ref  . drop (length "$"    ) $ s ) }
  $singleQuote $noneoln+ $singleQuote               { (\p s -> Text . chop '\''                  $ s ) }
  $doubleQuote $noneoln+ $doubleQuote               { (\p s -> Text . chop '\"'                  $ s ) }
  $nonunder $nonspecial* / $white                   { (\p s -> Text s                           ) }
  ^$semi $eoln [.\n]* $semi $eoln                   { (\p s -> Text . chopS ";\n"                  $ s ) }
  --^";\n" $nonsemi* $eoln ";\n"                    { (\p s -> Text . chopS ";\n"                  $ s ) }
  --^";\n" ( $nonsemi $noneolnsemi* $eoln )* ";\n"  { (\p s -> Text . chopS ";\n"                  $ s ) }

{

chopS :: String -> String -> String
chopS s t              = chopS' s s t
chopS' :: String -> String -> String -> String
chopS' []     s cs     = tailCutS s cs
chopS' (b:bs) s (c:cs) | b == c    = chopS' bs s cs 
chopS' (b:bs) s (c:cs) | otherwise = error $ "Cannot chop: " ++ show b ++ " <> " ++ show c
chopS' bs     s cs                 = error $ "Cannot chop: " ++ show bs ++ " from " ++ show cs

chop c (d:ds) | c == d = tailCut c ds
chop _ (_:ds)          = error "Cannot chop boundary characters!"
chop _ []              = error "String to short to chop!"

tailCutS bs     (c:cs) | length bs <= length cs = c:tailCutS bs cs
tailCutS (b:bs) (c:cs) | b == c                 = tailCutS bs cs
tailCutS []     []                              = []
tailCutS bs     cs                              = error $ "tailCutS " ++ show bs ++ " vs " ++ show cs
tailCut c [d]    | c == d  = []
tailCut c (b:bs)           = b:tailCut c bs

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

firstLine  = takeWhile (/= '\n')
--firstLines s = intersperse "\n" . take 2 . splitWith '\n' $ s

}
