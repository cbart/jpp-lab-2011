module Lab03Parser where
import Control.Monad.Error

instance Monad (Either a) where
  return = Right
  Right m >>= k = k m
  Left e >>= _ = Left e

data ParseError = Err {location::Integer, reason::String}
instance Error ParseError where
    noMsg = Err 0 "Unknown reason"
    strMsg s = Err ((words s) !! 2) "Parse error"

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Integer -> ParseMonad Integer
parseHexDigit c i = case c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'a' -> 10
    'b' -> 11
    'c' -> 12
    'd' -> 13
    'e' -> 14
    'f' -> 15
    _ -> error $ concat ["Character at ", show i, " wasnt matched a hex digit"]

parseHex :: String -> ParseMonad Integer
parseHex s = liftM (zip [0..]) $ foldrM (\el acc -> return (el:acc)) (return [])

toString :: Integer -> ParseMonad String
toString = return . show

convert :: String -> String
convert s = str where
    (Right str) = tryParse s `catchError` printError
    tryParse s = do {n <- parseHex s; toString n}
    printError e = 
