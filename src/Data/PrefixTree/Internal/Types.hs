module Data.PrefixTree.Internal.Types
  (
    PrefixTree (..)
  ) where

import           Data.IntMap.Strict (IntMap)

data PrefixTree v = Node (Maybe v) (IntMap (PrefixTree v))
  deriving (Show)


