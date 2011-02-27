module Main where

import Prelude hiding(lookup)
import Splay
import System.IO
import Data.Char
import Data.List(sortBy)
import System.Environment(getArgs)

parseString :: (Integral n) => String -> Dict String n
parseString = foldr addWord empty . validWords

addWord :: (Integral n) => String -> Dict String n -> Dict String n
addWord s dict = insert s c' dict
    where c' = maybe 1 (+1) $ lookup s dict

validWords :: String -> [String]
validWords s = filter (longerThan 2) $ words $ map letterOrSpace s

longerThan :: (Integral n) => n -> String -> Bool
longerThan 0 (_:_) = True
longerThan n (_:cs) = longerThan (n - 1) cs
longerThan _ [] = False

letterOrSpace :: Char -> Char
letterOrSpace c = if isLetter c then c else ' '

printTopWords :: (Integral n) => Dict String n -> IO ()
printTopWords dict = do
    putStr $ unlines $ map toString' $ take 10 $ sortBy (flip wordCounts) $ toList dict

toString' :: (Integral n) => (String, n) -> String
toString' (s, n) = s ++ " " ++ show n

wordCounts :: (Integral n) => (String, n) -> (String, n) -> Ordering
wordCounts (_, n) (_, m) = compare n m

main :: IO ()
main = do
    args <- getArgs
    fileHandle <- openFile (head args) ReadMode
    fileContent <- hGetContents fileHandle
    let dict = parseString fileContent
    printTopWords dict
    hClose fileHandle
