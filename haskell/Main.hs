module Main where

import Splay
import System.IO
import Data.List(sortBy)

hGetLine' :: Handle -> IO (Maybe String)
hGetLine' input = do
    eof <- hIsEOF input
    if eof
        then return Nothing
        else hGetLine input >>= return . Just

parseLines :: (Integral n) => Handle -> Dict String n -> IO (Dict String n)
parseLines input d =
    do
        mline <- hGetLine' input
        case mline of
            Nothing -> return d
            Just line -> parseLines input $ foldr countWord d $ filter longWord $ words line
    where
        countWord :: (Integral n) => String -> Dict String n -> Dict String n
        countWord w d = insert w count d
            where count = maybe 1 (+1) $ Splay.lookup w d
        longWord :: String -> Bool
        longWord (_:_:_) = True
        longWord _ = False

printLines :: (Integral n) => Handle -> [(String, n)] -> IO [(String, n)]
printLines _ [] = return []
printLines output (p:ps) = do
    hPutStrLn output $ toString p
    printLines output ps
    where
        toString :: (Integral n) => (String, n) -> String
        toString (s, n) = s ++ " " ++ show n

main :: IO ()
main = do
    parsed <- parseLines stdin empty
    let revSwapCompare b a = compare (snd a, fst a) (snd b, fst b)
    _ <- printLines stdout $ take 10 $ sortBy revSwapCompare $ toList parsed
    return ()
