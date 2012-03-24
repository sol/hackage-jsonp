{-# LANGUAGE DeriveDataTypeable #-}
module Package (Package (..)) where

import           Data.Data

data Package = Package {
  name    :: String
, version :: String
} deriving (Eq, Show, Data, Typeable)
