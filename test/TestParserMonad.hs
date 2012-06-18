{-# LANGUAGE TemplateHaskell #-}
module Main(main) where
import Test.QuickCheck
import Test.QuickCheck.All(quickCheckAll)

import ParserMonad
import Tokens(AlexPosn(..))

alexPos = AlexPn 0 0 0

prop_matchTypesValues1 = matchTypesValues [] [SStop alexPos] == Loop []

prop_matchTypesValues2 = (matchTypesValues [TSimple "type"] [SText alexPos "value",
                                                             SStop alexPos] ==
                          Loop [Entry "type" "value"])


main = $quickCheckAll


