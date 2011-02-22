module Splay
( Dict
, empty
, insert
, lookup
, fromList
, toList
, size
) where

import Prelude hiding(lookup)

data (Ord k) => Dict k v =
    Node (k, v) (Dict k v) (Dict k v)
  | Empty
    deriving (Show)

empty :: Dict k v
empty = Empty

insert :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert k v Empty = Node (k, v) Empty Empty
insert k v (Node p@(k', _) l r) = case compare k k' of
    LT -> Node p (insert k v l) r
    GT -> Node p l (insert k v r)
    EQ -> Node (k, v) l r

lookup :: (Ord k) => k -> Dict k v -> Maybe v
lookup k Empty = Nothing
lookup k (Node (k', v) l r) = case compare k k' of
    LT -> lookup k l
    GT -> lookup k r
    EQ -> Just v

fromList :: (Ord k) => [(k, v)] -> Dict k v
fromList = foldl (flip $ uncurry insert) Empty

toList :: (Ord k) => Dict k v -> [(k, v)]
toList Empty = []
toList (Node p l r) = (toList l) ++ [p] ++ (toList r)

size :: (Ord k) => Dict k v -> Int
size Empty = 0
size (Node _ l r) = 1 + size l + size r
