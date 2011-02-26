module Splay
( Dict
, empty
, insert
, lookup
, fromList
, toList
) where

import Prelude hiding(lookup)

data (Ord k) => Dict k v =
    Node (k, v) (Dict k v) (Dict k v)
  | Empty
    deriving (Show)

empty :: Dict k v
empty = Empty

foldDict :: (Ord k) => k -> Dict k v -> (a -> Dict k v -> a) -> a -> a
foldDict _ Empty _ a = a
foldDict k n@(Node (k', _) l r) f a = foldDict k s f (f a n)
    where s = case compare k k' of { LT -> l; GT -> r; EQ -> Empty }

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
toList (Node p l r) = (toList l) ++ (p : toList r)

--splay :: (Ord k) => k -> Dict k v -> Dict k v
--splay k Empty = Empty
--splay k d = zigzag LT >>