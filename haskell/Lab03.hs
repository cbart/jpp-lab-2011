module Lab03 where
import Control.Monad
import Network.CGI.Protocol (maybeRead)

-- Zadanie 1
readInts :: Integral n => Read n => String -> Either String [n]
readInts s = maybe (Left "Error") return $ foldr (liftM2 (:)) (return []) $ map maybeRead $ words s

sumInts :: String -> String
sumInts s = either id (unwords . map show) $ (readInts s) >>= (return . (:[]) . foldr (+) 0)

-- Zadanie 4
sequence1 :: Monad m => [m a] -> m [a]
-- sequence1 = foldl (liftM2 $ flip (:)) (return [])
sequence1 [] = return []
sequence1 (x:xs) = do
    x' <- x
    xs' <- sequence1 xs
    return (x':xs')


mapM1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1 f = sequence . map f

forM1 :: Monad m => [a] -> (a -> m b) -> m [b]
forM1 = flip mapM

-- Zadanie 5
allPairs1 :: [a] -> [a] -> [[a]]
allPairs1 xs ys = [[x, y] | x <- xs, y <- ys]

allPairs2 :: [a] -> [a] -> [[a]]
allPairs2 xs ys = do
    x <- xs
    y <- ys
    return [x, y]


