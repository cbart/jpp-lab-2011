ttake :: Int -> [a] -> [a]
ttake i l = rtake [] i l
    where
    rtake :: [a] -> Int -> [a] -> [a]
    rtake acc 0 l = acc
    rtake acc i [] = error "Empty list given"
    rtake acc i (lh:lt) = rtake (acc ++ [lh]) (i - 1) lt

hhead :: [a] -> a
hhead [] = error "Empty list given"
hhead (lh:lt) = lh

ttail :: [a] -> [a]
ttail [] = error "Empty list given"
ttail (lh:lt) = lt

ddrop :: Int -> [a] -> [a]
ddrop 0 l = l
ddrop i [] = error "Too short list given"
ddrop i (lh:lt) = ddrop (i - 1) lt

rreverse :: [a] -> [a]
rreverse l = rrreverse [] l
    where
    rrreverse acc [] = acc
    rrreverse acc (lh:lt) = rrreverse (lh:acc) lt

iinits :: [b] -> [[b]]
iinits [] = [[]]
iinits (h:hs) = [] : map (h : ) (iinits hs)

ppartitions :: [a] -> [([a], [a])]
ppartitions [] = [([], [])]
ppartitions (h:hs) = (([], (h:hs)) : (map (\(x, y) -> ((h:x), y)) (ppartitions hs)))

zpartitions :: [a] -> [([a], [a])]
zpartitions l = zip (iinits l) (map rreverse (iinits (rreverse l)))

ppermutations :: [a] -> [[a]]
ppermutations [] = [[]]
ppermutations (h:hs) = concatMap (anywhere h) (ppermutations hs)
    where
    anywhere x [] = [[x]]
    anywhere x (h:hs) = (x:h:hs):(map (h:) (anywhere x hs))

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (h:hs) = h : (nub (filter (/= h) hs))
