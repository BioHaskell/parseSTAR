{
module Main(main) where
--module Tokens (Token(..), alexGScan) where
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
  $white +                                 ;
  $commentChar .* $eoln                    ;
  $under $nonspecial*   / $white              { tok (\p s -> Name p . drop (length "_") $ s ) }  
  "save_" $nonspecial+                     { tok (\p s -> Save p . drop (length "save_") $ s) }
  "save_"   / $white                       { tok (\p s -> EndSave p ) }
  "stop_"                                  { tok (\p s -> EndLoop p ) }
  "loop_"                                  { tok (\p s -> Loop p    ) }
  "global_"                                { tok (\p s -> Global p  ) }
  "data_" $nonwhite+ / $white              { tok (\p s -> Data p . drop (length "data_") $ s ) }
  $dollar $nonwhite+ / $white              { tok (\p s -> Ref  p . drop (length "$"    ) $ s ) }
  $singleQuote $noneoln+ $singleQuote      { tok (\p s -> Text p . chop '\'' $ s ) }
  $doubleQuote $noneoln+ $doubleQuote      { tok (\p s -> Text p . chop '\"'  $ s ) }
  $nonunder $nonspecial* / $white          { tok (\p s -> Text p s ) }
  ^$semi $eoln [.\n]* $semi $eoln { tok (\p s -> Text p . chopS ";\n" $ s ) }
  --^";\n" $nonsemi* $eoln ";\n"             { tok (\p s -> Text p . chopS ";\n" $ s ) }
  --^";\n" ( $nonsemi $noneolnsemi* $eoln )* ";\n"  { tok (\p s -> Text p . chopS ";\n" $ s ) }

{

tok f p s = f p s

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

myScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> do print $ act pos (take len str) 
                                             go inp'

-- Test
main = do
  s <- getContents
  let startInput = (AlexPn 0 0 0, '\n', [], s)
  myScanTokens s
}
