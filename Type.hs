{-# LANGUAGE NamedFieldPuns #-}
module Type(
       STAR     (..),
       STARBlock(..),
       STAREntry(..),
       STARDict (..),
       STARKey,
) where

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
               | Frame { key          :: String,
                         frameEntries :: [STAREntry]
                       }
               | Loop  { table        :: [STARDict]
                       }
  deriving (Show, Eq)

newtype STARDict = STARDict { unSTARDict :: [(String, String)] }
  deriving (Show, Eq)


