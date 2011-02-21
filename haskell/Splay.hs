module Splay
( empty1
, insert1
, lookup1
, fromList1
, toList1
) where

data (Ord k) => Dict k v = Node (k, v) (Dict k v) (Dict k v) | Empty deriving (Show)

empty1 :: Dict k v
empty1 = Empty

insert1 :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert1 k v Empty = Node (k, v) Empty Empty
insert1 k v (Node p@(k', _) l r)
    | k == k' = Node (k, v) l r
    | k < k' = Node p (insert1 k v l) r
    | otherwise = Node p l (insert1 k v r)

lookup1 :: (Ord k) => k -> Dict k v -> Maybe v
lookup1 k Empty = Nothing
lookup1 k (Node (k', v) l r)
    | k == k' = Just v
    | k < k' = lookup1 k l
    | otherwise = lookup1 k r

fromList1 :: (Ord k) => [(k, v)] -> Dict k v
fromList1 = foldl (flip $ uncurry insert1) Empty

toList1 :: (Ord k) => Dict k v -> [(k, v)]
toList1 Empty = []
toList1 (Node p l r) = (toList1 l) ++ [p] ++ (toList1 r)
