{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding(lookup)
import Splay
import System.IO
import Data.Char
import Data.List(sortBy)
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    file_content <- readFile $ head args
    let dict = countWords file_content
    let top_ten = take 10 $ topCountedWords dict
    putStr $ formatLines top_ten

countWords :: Integral n => String -> Dict String n
countWords = foldr addWord empty . onlyValidWords . getWords

addWord :: forall n. Integral n => String -> Dict String n -> Dict String n
addWord s dict = insert s count_s dict
    where
        count_s :: n
        count_s = maybe 1 (+1) $ lookup s dict

onlyValidWords :: [String] -> [String]
onlyValidWords = filter (longerThan 2)
    where
        longerThan :: Integral n => n -> String -> Bool
        longerThan 0 (_:_) = True
        longerThan n (_:cs) = longerThan (n - 1) cs
        longerThan _ [] = False

getWords :: String -> [String]
getWords = words . map letterOtherwiseSpace
    where
        letterOtherwiseSpace :: Char -> Char
        letterOtherwiseSpace c = if isLetter c then c else ' '

topCountedWords :: Integral n => Dict String n -> [(String, n)]
topCountedWords = sortBy (flip compareByOccurenceCount) . toList
    where
        compareByOccurenceCount :: Integral n => (String, n) -> (String, n) -> Ordering
        compareByOccurenceCount (_, n) (_, m) = compare n m

formatLines :: Integral n => [(String, n)] -> String
formatLines = unlines . map formatPair
    where
        formatPair :: Integral n => (String, n) -> String
        formatPair (s, n) = concat [s, " ", show n]
