{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.STAR.Path((./), (/<>), (->//)
                     ,starBlocks
                     ,filterP
                     ,blocksByName
                     ,entriesByName
                     ,entryValue
                     ,allEntriesByName
                     ,flattenEntries)
where

import Prelude hiding(String)
import Data.STAR.Type

-- ^ This module presents a convenient, compositional interface for filtering
--   Example use:
--   > test1 :: STAR -> [String]
--   > test1 = blocksByName "" ./ entriesByName "chemical_shifts" ./ entryValue
--   Another example:
--   > test2 :: STAR -> [String]
--   > test2 = starBlocks ./ entries ./ entryValue

-- | Path separator - serves as general function composition operator.
infixr 3 ./
(./) :: (a -> [a1]) -> (a1 -> [b]) -> a -> [b]
(./) = \f g -> concatMap g . f

-- | Path predicate operator - serves as general function composition operator.
(/<>) ::  (a -> b) -> (b -> Bool) -> a -> [b]
(/<>) = \f g -> filterP g . f

infixl 1 ->//
-- | Applies a path to a set of elements (not just a document, or single entry.)
elts ->// path = concatMap path elts

starBlocks ::  STAR -> [STARBlock]
starBlocks (STAR blocks) = blocks

filterP ::  (t -> Bool) -> t -> [t]
filterP f x = if f x then [x] else []
--  STAR selectors are typed, and always return a list of results
--  (in order to be conveniently used with concatMap and composition.)
-- | Selects all block matching a given name or GLOBAL block for empty string.
--   TODO: Regex for names?
blocksByName ::  String -> STAR -> [STAREntry]
blocksByName name = starBlocks ./ filterP matches ./ entries
  where
    matches (Global _)               = name ==""
    matches (Data { dataKey = key }) = key == name


-- | Filter entries with a given name. Expand loops if necessary.
entriesByName ::  String -> STAREntry -> [STAREntry]
entriesByName name es = matches es
  where
    matches (Loop table) = concatMap (concatMap matches) table
    matches e            = if key e == name
                             then [e]
                             else []

-- | Extracts a string value from a flat entry.
entryValue ::  STAREntry -> [String]
entryValue (Loop  _)   = []
entryValue (Frame _ _) = []
entryValue e           = [value $ e]

-- | Expands a possibly complex entry into a list of flat entries.
flattenEntries :: STAREntry -> [STAREntry]
flattenEntries (Loop    l) = concat l
flattenEntries (Frame _ l) = l
flattenEntries e           = [e]

-- | Find all flat entries with a given name, and return their values. 
allEntriesByName name = starBlocks ./ entries ./ flattenEntries ./ entriesByName name ./ entryValue
