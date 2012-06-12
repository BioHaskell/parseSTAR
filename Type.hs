{-# LANGUAGE NamedFieldPuns #-}
module Type(
       STAR     (..),
       STARBlock(..),
       STAREntry(..),
       STARKey,
       String
) where

import Data.ByteString.Lazy.Char8 as BSC
import Prelude hiding(String)

type String = BSC.ByteString

newtype STAR = STAR [STARBlock]
  deriving (Show, Eq)

data STARBlock = Global { entries     :: [STAREntry]
                        }
               | Data   { dataKey     :: String,
                          entries     :: [STAREntry]
                        }
  deriving (Show, Eq)

type STARKey = String

data STAREntry = Entry { key          :: String,
                         value        :: String
                       }
               | Ref   { key          :: String,
                         value        :: String
                       }
               | Frame { key          :: String,
                         frameEntries :: [STAREntry]
                       }
               | Loop  { table        :: [STAREntry]
                       }
  deriving (Show, Eq)


