{
module Main(main) where
--module Tokens (Token(..), alexGScan) where
-- Question: case insensitive?
}

%wrapper "gscan"

$white       = [ \  \t \n \f \v \r ]
$nonwhite    = ~ $white # [ \# \! ] -- non-whitespace
$nonspecial  = $nonwhite # [ \_ \' \" \$ ] -- not whitespace and not underline
$nonunder    = $nonspecial # \_ -- not whitespace and not underline
$commentChar = [ \! \# ]
$under       = [ \_ ]
$dollar      = \$ 
$eoln        = \n
$noneoln     = [^\n]
$singleQuote = [\']
$doubleQuote = [\"]

tokens :-
  $white +                              { tok (\p s -> White p ) }
  $under $nonspecial / $white           { tok (\p s -> Name p . drop (length "_") $ s ) }  
  "save_" $nonwhite+ / $white           { tok (\p s -> Save p . drop (length "save_") $ s) }
  "save_"   / $white                    { tok (\p s -> EndSave p ) }
  "stop_"   / $white                    { tok (\p s -> EndLoop p ) }
  "loop_"   / $white                    { tok (\p s -> Loop p    ) }
  "global_" / $white                    { tok (\p s -> Global p  ) }
  "data_" $nonwhite+ / $white           { tok (\p s -> Data p . drop (length "data_") $ s ) }
  $dollar $nonwhite+ / $white           { tok (\p s -> Ref  p . drop (length "$"    ) $ s ) }
  $commentChar .* $eoln                   { tok (\p s -> Comment p s ) }
  $singleQuote $noneoln+ $singleQuote   { tok (\p s -> Text p . chop '\'' $ s ) }
  $doubleQuote $noneoln+ $doubleQuote   { tok (\p s -> Text p . chop '"'  $ s ) }
  $nonunder $nonspecial* / $white       { tok (\p s -> Text p s ) }
--  ^";\n" ( [^";"$noneoln $eoln )* /";\n"  { tok (\p s -> Text . chop ";\n" s ) }

{

tok f p c str len cont (sc,state) = f p (take len str) : cont (sc + len,state)
-- tok f p c str len cont (sc,state) = f p (take len str) : cont (sc + len,state)

chop c (d:ds) | c == d = tailCut c ds
chop _ (_:ds)          = error "Cannot chop boundary characters!"
chop _ []              = error "String to short to chop!"

tailCut c [d]    | c == d  = []
tailCut c (b:bs)           = b:tailCut c bs

-- The token type:
data Token =
        White   AlexPosn         |
        Name    AlexPosn String  |
        Text    AlexPosn String  |
        Comment AlexPosn String  |
        Save    AlexPosn String  |
        EndSave AlexPosn         |
        Loop    AlexPosn         |
        EndLoop AlexPosn         |
        Data    AlexPosn String  |
        Global  AlexPosn         |
        Ref     AlexPosn String  |
        Err     String AlexPosn String
  deriving (Eq,Show)

firstLine  = takeWhile (/= '\n')
--firstLines s = intersperse "\n" . take 2 . splitWith '\n' $ s

-- Test
main = do
  s <- getContents
  print (alexGScan stop "<stdin>" s)
  where
        stop p c "" (sc,f) = []
        stop p c s  (sc,f) = error $ "Unknown lexeme " ++ show (take 100 s) ++ " at line " ++ show sc ++ " of " ++ f
}
