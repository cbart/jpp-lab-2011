module Main where
import Splay
import Data.List
import Data.Traversable
import Test.HUnit

testInsertPermutations n = TestCase (do
    let test l = assertEqual "Lists should match" (zip [1..n] [1..n]) $ toList $ fromList $ zip l l
    foldl (\_ -> test) (return ()) $ permutations [1..n])

main :: IO ()
main = do
    _ <- forM [1..10] $ runTestTT . testInsertPermutations
    return ()
