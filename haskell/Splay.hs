module Splay
( empty
, insert
, lookup
, fromList
, toList
) where

import Prelude hiding(lookup)

data (Ord k) => Dict k v = Node (k, v) (Dict k v) (Dict k v) | Empty deriving (Show)

empty :: Dict k v
empty = Empty

insert :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert k v Empty = Node (k, v) Empty Empty
insert k v (Node p@(k', _) l r)
    | k < k' = Node p (insert k v l) r
    | k > k' = Node p l (insert k v r)
    | otherwise = Node (k, v) l r

lookup :: (Ord k) => k -> Dict k v -> Maybe v
lookup k Empty = Nothing
lookup k (Node (k', v) l r)
    | k < k' = lookup k l
    | k > k' = lookup k r
    | otherwise = Just v

fromList :: (Ord k) => [(k, v)] -> Dict k v
fromList = foldl (flip $ uncurry insert) Empty

toList :: (Ord k) => Dict k v -> [(k, v)]
toList Empty = []
toList (Node p l r) = (toList l) ++ [p] ++ (toList r)
