module Lab04
( r1
) where
import Control.Monad.Reader
import Control.Monad.State

r1 :: Reader Int String
r1 = do
    x <- ask
    return $ show (x+1)

r2 :: Reader Int String
r2 = (Reader id) >>= (\x -> (Reader (\_ -> show (x+1))))

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

nextNum :: State Int Int
nextNum = State $ \i -> (i, i+1)

renumberNode :: Tree a -> State Int (Tree Int)
renumberNode (Node a l r) = do
    a' <- nextNum
    l' <- renumberNode l
    r' <- renumberNode r
    return (Node a' l' r')
renumberNode Empty = return Empty

renumberTree :: Tree a -> Tree Int
renumberTree t = evalState (renumberNode t) 0
