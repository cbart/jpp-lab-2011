module Lab01 where

id1 :: a -> a
id1 a = a  -- Returns its argument

fst1 :: (a, b) -> a
fst1 (a, b) = a  -- Returns first element

snd1 :: (a, b) -> b
snd1 (a, b) = b  -- Return second element

pred1 :: (Num n) => n -> n
pred1 n = let { n_pred = n - 1 } in id $! n_pred

length3 :: [a] -> Integer
length3 l = length3rec l 0
    where
        length3rec :: (Num n) => [a] -> n -> n  -- Returns n + length a
        length3rec [] n = n
        length3rec (_:xs) n = length3rec xs $! (n + 1)

take1 :: (Num n) => n -> [a] -> [a]
take1 n l = reverse1 $ snd1 $ foldl take1rec (n, []) l
    where
        take1rec :: (Num n) => (n, [a]) -> a -> (n, [a])
        take1rec (0, l) _ = (0, l)
        take1rec (n, l) a = (pred1 n, a:l)

head1 :: [a] -> a
head1 [] = error "Empty list given"
head1 (lh:lt) = lh

tail1 :: [a] -> [a]
tail1 [] = error "Empty list given"
tail1 (lh:lt) = lt

last1 :: [a] -> a
last1 = head1 . reverse1

init1 :: [a] -> [a]
init1 = reverse1 . tail1 . reverse1

drop1 :: (Num n) => n -> [a] -> [a]
drop1 0 l = l
drop1 i [] = error "Too short list given"
drop1 i (lh:lt) = drop1 (i - 1) lt

reverse1 :: [a] -> [a]
reverse1 = foldl (flip (:)) []

inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 (x:xs) = [] : map (x : ) (inits1 xs)

tails1 :: [a] -> [[a]]
tails1 l = foldl (\t _ -> (tail1 $ head1 t):t) [l] l

inits2 :: [a] -> [[a]]
inits2 l = map reverse1 $ tails1 $ reverse1 l

partitions1 :: [a] -> [([a], [a])]
partitions1 l = [(left, right)
    | left <- inits1 l,
      right <- tails1 l,
      length3 l == length3 left + length3 right]

partitions2 :: [a] -> [([a], [a])]
partitions2 [] = [([], [])]
partitions2 (h:hs) = (([], (h:hs)) : (map (\(x, y) -> ((h:x), y)) (partitions2 hs)))

partitions3 :: [a] -> [([a], [a])]
partitions3 l = zip (inits1 l) $ map reverse1 $ inits1 $ reverse1 l

permutations1 :: [a] -> [[a]]  -- Returns list of permutations
permutations1 [] = [[]]
permutations1 (h:hs) = concatMap (anywhere1 h) (permutations1 hs)
    where
    anywhere1 e [] = [[e]]
    anywhere1 e (x:xs) = (e:x:xs) : (map (x:) $ anywhere1 e xs)

nub1 :: (Eq a) => [a] -> [a]  -- Returns list of unique elements
nub1 [] = []
nub1 (h:hs) = h : (nub1 $ filter (/= h) hs)

triads2 :: (Num n, Enum n) => n -> [(n, n, n)]
triads2 n = [(x, y, z) |
    x <- [0..n], y <- [0..n], z <- [0..n],
    square x + square y == square z]
    where
        square :: (Num n) => n -> n
        square n = n * n

factorial1 :: (Num n) => n -> n
factorial1 = factorial1rec 1
    where
        factorial1rec :: (Num n) => n -> n -> n
        factorial1rec acc 0 = acc
        factorial1rec acc n = let
                newAcc = acc * n
                newN = n - 1
            in seq newAcc $ seq newN $ factorial1rec newAcc newN

factorial2 :: (Num n) => n -> Integer
factorial2 n = length3 $ permutations1 (take1 n [1..])

fibonacci1 :: (Num n) => n -> n
fibonacci1 n = fibonacci1rec 0 1 n
    where
        fibonacci1rec :: (Num n) => n -> n -> n -> n
        fibonacci1rec a b 0 = a
        fibonacci1rec a b n = let
                aPlusB = a + b
                nPred = n - 1
            in aPlusB `seq` nPred `seq` fibonacci1rec b aPlusB nPred

indexOf1 :: (Num n) => Char -> String -> Maybe n
indexOf1 c s = indexOf1rec 0 c s
    where
        indexOf1rec :: (Num n) => n -> Char -> String -> Maybe n
        indexOf1rec _ _ [] = Nothing
        indexOf1rec i c (s:ss)
            | c == s = Just i
            | otherwise = indexOf1rec (i + 1) c ss

indexOf2 :: Char -> String -> Maybe Int
indexOf2 c s = lookup c $ zip s [0,1..]

indexOf3 :: Char -> String -> Maybe Int
indexOf3 c [] = Nothing
indexOf3 c (s:ss)
    | c == s = Just 0
    | otherwise = indexOf3 c ss >>= Just . (+1)

positions1 :: Char -> String -> [Int]
positions1 c s = map snd $ filter ((c==) . fst) $ zip s [0, 1..]

smain :: String -> String
smain = id

--main = interact smain
