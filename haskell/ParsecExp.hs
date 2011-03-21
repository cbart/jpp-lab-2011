module ParsecExp where

import Control.Monad
import Text.ParserCombinators.Parsec

data Exp =
    Integer Integer
  | Operator Binop Exp Exp
  | Value String
  | Let String Exp Exp

type Binop = Int -> Int -> Int

readExp :: Parser Exp
readExp =
    readInteger
    <|> readVal
    <|> readLet
    <|> readBinop

readInteger :: Parser Exp
readInteger = liftM Integer decimal

readBinop :: Parser Exp
readBinop = do
    operatorName <- oneOf "+-"
    skipMany1 space
    arg1 <- readExp
    skipMany1 space
    arg2 <- readExp
    return $ binop operatorName arg1 arg2

binop :: Char -> Exp -> Exp -> Exp
binop '+' = Operator (+)
binop '-' = Operator (-)

readVal :: Parser Exp
readVal = do
    valueName <- strinLiteral
    return Value valueName

readLet :: Parser Exp
readLet = do
    readLetKeyword
    exps <- many readExp


space :: Parser Char
space = char ' '

spaces :: Parser String
spaces = many space
