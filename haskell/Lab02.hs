module Lab02 where

import Lab01

-- Zadanie 1

-- Point-free style
incAll1 :: (Num n, Functor f, Functor f1) => (f (f1 n)) -> (f (f1 n))
incAll1 = fmap $ fmap (+1)

factorial3 :: (Num n, Enum n) => n -> n
factorial3 n = foldl (*) 1 [1..n]

concat1 :: [[a]] -> [a]
concat1 = foldr (++) []

-- Zadanie 2

data Tree a =
      Empty
    | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

instance Functor Tree where
    --fmap :: (a -> b) -> (Tree a) -> (Tree b)
    fmap _ Empty = Empty
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

-- infix order
foldti :: (a -> b -> a) -> a -> Tree b -> a
foldti f acc Empty = acc
foldti f acc (Node el left right) = foldti f (foldti f (f acc el) left) right

-- infix order
toList1 :: Tree a -> [a]
toList1 = reverse1 . foldti (flip (:)) []

-- BST
insert1 :: (Ord a) => a -> Tree a -> Tree a
insert1 a Empty = Node a Empty Empty
insert1 a (Node e l r)
    | a <= e = Node e (insert1 a l) r
    | otherwise = Node e l (insert1 a r)

contains1 :: (Ord a) => a -> Tree a -> Bool
contains1 _ Empty = False
contains1 a (Node e l r)
    | a <= e = contains1 a l
    | otherwise = contains1 a r

fromList1 :: (Ord a) => [a] -> Tree a
fromList1 = foldl (flip insert1) Empty

-- prefix order
foldtp :: (a -> b -> a) -> a -> Tree b -> a
foldtp f acc Empty = acc
foldtp f acc (Node e l r) = foldtp f (f (foldtp f acc l) e) r

toList2 :: Tree a -> [a]
toList2 = reverse1 . foldtp (flip (:)) []

sort1 :: (Ord a) => [a] -> [a]
sort1 = toList2 . fromList1

qsort1 :: (Ord a) => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) = (sort2 (filter (<=x) xs)) ++ [x] ++ (sort2 (filter (>x) xs))
