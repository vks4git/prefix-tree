module Data.PrefixTree.Internal.Utils
  (
    member
  , insert
  , insertKV
  , adjust
  , find
  , fromList
  , empty
  ) where

import           Data.Char                      (ord)
import qualified Data.IntMap.Strict             as M
import           Data.PrefixTree.Internal.Types (PrefixTree (..))

member :: String -> PrefixTree v -> Bool
member [] (Node (Just _) _) = True
member (x:xs) (Node _ imap) = case M.lookup (ord x) imap of
                                Just tree -> member xs tree
                                _         -> False
member _ _ = False

insert :: String -> v -> PrefixTree v -> PrefixTree v
insert [] val (Node _ imap) = Node (Just val) imap
insert (x:xs) val (Node v imap) = Node v newImap
  where
    newImap = case M.lookup (ord x) imap of
                Nothing -> M.insert (ord x) (insert xs val empty) imap
                _       -> M.update (Just . insert xs val) (ord x) imap

insertKV :: (String, v) -> PrefixTree v -> PrefixTree v
insertKV (key, val) = insert key val

find :: String -> PrefixTree v -> Maybe v
find [] (Node maybeVal _) = maybeVal
find (x:xs) (Node _ imap) = case M.lookup (ord x) imap of
                              Just tree -> find xs tree
                              _         -> Nothing

fromList :: [(String, v)] -> PrefixTree v
fromList = foldr insertKV empty

adjust :: (v -> v) -> String -> PrefixTree v -> PrefixTree v
adjust f [] (Node maybeVal imap) = Node (f <$> maybeVal) imap
adjust f (x:xs) (Node v imap) = Node v newImap
  where
    newImap = case M.lookup (ord x) imap of
                Nothing -> imap
                _       -> M.update (Just . adjust f xs) (ord x) imap

empty :: PrefixTree v
empty = Node Nothing M.empty

