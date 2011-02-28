module Lab03 where
import Control.Monad
import Network.CGI.Protocol (maybeRead)

instance Monad (Either a) where
    return = Right
    Right m >>= k = k m
    Left e >>= _ = Left e

readInts :: (Integral n) => (Read n) => String -> Either String [n]
readInts s = foldr allRights (return []) $ map eitherRead $ words s
    where
        eitherRead :: (Read a) => String -> Either String a
        eitherRead s = maybe (Left "Error") return $ maybeRead s
        allRights :: (Integral n) => Either String n -> Either String [n] -> Either String [n]
        allRights e acc = (>>=) acc $ \l -> (>>=) e $ Right . (:l)

sumInts :: String -> String
sumInts s = either id (unwords . map show) $ (readInts s) >>= (return . (:[]) . foldr (+) 0)

--readList2 :: (Read a) => String -> Maybe [a]
--readList2 = lift2M (:) . map maybeRead . words
