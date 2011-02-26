module Splay
( Dict
, empty
, insert
, lookup
, splay
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

insert :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert k v d = splay k $ insert' k v d
    where
        insert' :: (Ord k) => k -> v -> Dict k v -> Dict k v
        insert' k v Empty = Node (k, v) Empty Empty
        insert' k v (Node p@(k', _) l r) = case compare k k' of
            LT -> Node p (insert k v l) r
            GT -> Node p l (insert k v r)
            EQ -> Node (k, v) l r

lookup :: (Ord k) => k -> Dict k v -> Maybe v
lookup k n = case splay k n of
    Empty -> Nothing
    (Node (k', v) _ _) -> if k == k' then Just v else Nothing

fromList :: (Ord k) => [(k, v)] -> Dict k v
fromList = foldl (flip $ uncurry insert) Empty

toList :: (Ord k) => Dict k v -> [(k, v)]
toList Empty = []
toList (Node p l r) = (toList l) ++ (p : toList r)

splay :: (Ord k) => k -> Dict k v -> Dict k v
splay k Empty = Empty
splay k n@(Node (k', _) _ _) = case compare k k' of
    EQ -> n
    o -> case node o n of
        Empty -> n
        nn@(Node (k'', _) _ _) -> case compare k k'' of
            EQ -> rot1 o n
            oo -> case node oo nn of
                Empty -> rot1 o n
                (Node _ _ _) -> rot2 o oo n $ splay k

node :: (Ord k) => Ordering -> Dict k v -> Dict k v
node LT (Node _ l _) = l
node GT (Node _ _ r) = r

rot1 :: (Ord k) => Ordering -> Dict k v -> Dict k v
rot1 LT (Node p (Node lp ll lr) r) = Node lp ll (Node p lr r)
rot1 GT (Node p l (Node rp rl rr)) = Node rp (Node p l rl) rr
rot1 _ _ = error "No such rotation"

rot2 :: (Ord k) => Ordering -> Ordering -> Dict k v -> (Dict k v -> Dict k v) -> Dict k v
rot2 LT LT (Node p (Node lp ll lr) r) splay' = case splay' ll of
    (Node llp lll llr) -> Node llp lll (Node lp llr (Node p lr r))
rot2 LT GT (Node p (Node lp ll lr) r) splay' = case splay' lr of
    (Node lrp lrl lrr) -> Node lrp (Node lp ll lrl) (Node p lrr r)
rot2 GT LT (Node p l (Node rp rl rr)) splay' = case splay' rl of
    (Node rlp rll rlr) -> Node rlp (Node p l rll) (Node rp rlr rr)
rot2 GT GT (Node p l (Node rp rl rr)) splay' = case splay' rr of
    (Node rrp rrl rrr) -> Node rrp (Node rp (Node p l rl) rrl) rrr
