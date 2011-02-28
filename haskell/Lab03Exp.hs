module Lab03Exp where
import Prelude hiding(lookup)
import Data.Map
import Data.Maybe
import Control.Monad.Reader

data Exp =
    IntE Int  -- integer
  | OpE Op Exp Exp  -- operator
  | VarE String  -- var
  | LetE String Exp Exp  -- let var = exp_1 in exp_2

type Op = Int -> Int -> Int

type Environment = Map String Exp

evalExp :: Exp -> Int
evalExp expr = runReader (evalE expr) empty

evalE :: Exp -> Reader Environment Int
evalE (IntE i) = return i
evalE (OpE op e1 e2) = do
  i1 <- evalE e1
  i2 <- evalE e2
  return $ op i1 i2
evalE (VarE s) = do
  e <- asks $ fromJust . lookup s
  evalE e
evalE (LetE s e1 e2) = do
  local (insert s e1) $ evalE e2

test = LetE "x" (LetE "y" (OpE (+) (IntE 5) (IntE 6))
                      (OpE div y (IntE 5)))
                (OpE (*) x (IntE 3))
    where x = VarE "x"
          y = VarE "y"
