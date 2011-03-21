{-# LANGUAGE FlexibleInstances #-}
module StateTParser(Parser,runParser,item) where
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Identity

type Parser a = (StateT [Char] (MaybeT Identity)) a

runParser = runStateT

item :: Parser Char
item =
    do
        input <- get
        case input of
            [] -> mzero
            (x:xs) -> put xs >> return x

instance Show a => Show (MaybeT Identity a) where
    show = show . runIdentity . runMaybeT

-- Use the StateT transformer on Maybe
-- type Parser a = (StateT [Char] Maybe) a
-- runParser = runStateT
