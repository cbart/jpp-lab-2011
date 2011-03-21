{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses #-}
module StateParser(
  Parser(..),    -- * -> *
  -- runParser,     -- Parser a -> (String -> m (a,String))
  item           -- Parser Char
  ) where
import Control.Monad.State
import Data.Char(isDigit,digitToInt)

-- Manually combine State and Maybe
newtype Parser a = Parser {
   runParser :: [Char] -> Maybe(a,[Char])
   }
mkParser :: ([Char] -> Maybe(a,[Char])) -> Parser a
mkParser = Parser

instance Monad Parser where
  return a = Parser $ \s -> Just (a,s)
  (Parser f) >>= k = Parser $ bind f k where
    bind f k s = case (f s) of
     Nothing     -> Nothing
     Just (a,s') -> runParser (k a) s'

-- class MonadState m s | m -> s where
--     get :: m s
--     put :: s -> m ()

instance MonadState [Char] Parser where
  get = Parser $ \s -> Just (s,s)
  put s = Parser $ \ _ -> Just ((),s)

instance Functor Parser where
  -- fmap :: (a->b) -> Parser a -> Parser b
  fmap = liftM

zero :: Parser a
zero = mkParser $ \_-> mzero

plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = mkParser $
  \xs -> runParser p1 xs `mplus` runParser p2 xs

-- plus p1 p2 = Parser $ \xs -> case runParser p1 xs of
--   Nothing -> runParser p2 xs
--   result -> result

instance MonadPlus Parser where
  mzero = zero
  mplus = plus

item1 :: Parser Char
item1 = Parser f where
  f [] = Nothing
  f (x:xs) = Just (x,xs)

item2 = do
  input <- get
  case input of
    [] -> zero
    (x:xs) -> put xs >> return x
item = item2

