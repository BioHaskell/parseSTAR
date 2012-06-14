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
import Control.DeepSeq

type String = BSC.ByteString

newtype STAR = STAR [STARBlock]
  deriving (Show, Eq)

data STARBlock = Global { entries     :: ![STAREntry]
                        }
               | Data   { dataKey     :: !String,
                          entries     :: ![STAREntry]
                        }
  deriving (Show, Eq)

instance NFData STARBlock
  where
    rnf (Global   es) = rnf es
    rnf (Data   k es) = rnf es

type STARKey = String

data STAREntry = Entry { key          :: !String,
                         value        :: !String
                       }
               | Ref   { key          :: !String,
                         value        :: !String
                       }
               | Frame { key          :: !String,
                         frameEntries :: ![STAREntry]
                       }
               | Loop  { table        :: ![STAREntry]
                       }
  deriving (Show, Eq)

instance NFData STAREntry
  where
    rnf (Frame k vs) = rnf vs
    rnf (Loop    vs) = rnf vs
    rnf (Entry  _ _) = ()
    rnf (Ref    _ _) = ()



