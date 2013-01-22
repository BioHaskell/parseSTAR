{-# LANGUAGE NamedFieldPuns, CPP #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
module Data.STAR.Type(
       STAR     (..),
       STARBlock(..),
       STAREntry(..),
       STARKey(..),
       String(..)
) where

import Data.ByteString.Char8 as BSC
import Prelude hiding(String)
import Control.DeepSeq
import Data.Binary

type String = BSC.ByteString

newtype STAR = STAR [STARBlock]
  deriving (Show, Eq)

data STARBlock = Global { entries     :: ![STAREntry]
                        }
               | Data   { dataKey     :: !String,
                          entries     :: ![STAREntry]
                        }
  deriving (Show, Eq)

#ifdef DEFINE_NFDATA_BYTESTRING
instance NFData BSC.ByteString
  where
    rnf bs = bs `seq` ()
#endif

{-!

deriving instance Binary STARBlock
deriving instance Binary STAR
deriving instance Binary STAREntry

deriving instance NFData STARBlock
deriving instance NFData STAR
deriving instance NFData STAREntry
 !-}

{-
instance NFData STARBlock
  where
    rnf (Global   es) = rnf es
    rnf (Data   k es) = rnf es
-}

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
               | Loop  { table        :: ![[STAREntry]]
                       }
  deriving (Show, Eq)

{-
instance NFData STAREntry
  where
    rnf (Frame k vs) = rnf vs
    rnf (Loop    vs) = rnf vs
    rnf (Entry  _ _) = ()
    rnf (Ref    _ _) = ()
-}


