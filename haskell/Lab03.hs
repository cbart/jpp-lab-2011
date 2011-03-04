module Lab03 where
import Control.Monad
import Network.CGI.Protocol (maybeRead)

instance Monad (Either a) where
  return = Right
  Right m >>= k = k m
  Left e >>= _ = Left e

-- Zadanie 1
readInts :: Integral n => Read n => String -> Either String [n]
readInts s = maybe (Left "Error") return $ foldr (liftM2 (:)) (return []) $ map maybeRead $ words s

sumInts :: String -> String
sumInts s = either id (unwords . map show) $ (readInts s) >>= (return . (:[]) . foldr (+) 0)

-- Zadanie 4
sequence1 :: Monad m => [m a] -> m [a]
sequence1 l = reversed >>= (return . reverse)
  where
    reversed = foldl (liftM2 $ flip (:)) (return []) l

-- Zadanie 5
allPairs1 :: [a] -> [a] -> [[a]]
allPairs1 xs ys = [[x, y] | x <- xs, y <- ys]

allPairs2 :: [a] -> [a] -> [[a]]
allPairs2 xs ys = do
  x <- xs
  y <- ys
  return [x, y]


