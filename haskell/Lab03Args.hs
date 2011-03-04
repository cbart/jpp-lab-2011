module Main where
import System.Environment(getArgs)
import Control.Monad(forM_)

main :: IO ()
main = do
    args <- getArgs
    forM_ args putStrLn
