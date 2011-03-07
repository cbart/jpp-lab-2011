module Lab03Parser where
import Control.Monad.Error
import Data.Char

--instance Monad (Either a) where
--  return = Right
--  Right m >>= k = k m
--  Left e >>= _ = Left e

data ParseError = Err {location::Integer, reason::String}
instance Error ParseError where
    noMsg = Err 0 "Unknown reason"
    strMsg s = Err (read $ (words s) !! 2) "Parse error"

niceMessage :: ParseError -> String
niceMessage (Err i r) = concat ["Error at col: ", show i, "; with message: ", r]

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Integer -> ParseMonad Integer
parseHexDigit c i =
    if isHexDigit c
        then return $ toInteger c_i
        else throwError $ strMsg $ concat ["Character at ", show i, " wasnt matched a hex digit"]
    where
        c_i :: Int
        c_i = if '0' <= c && c <= '9'
            then ord c - ord '0'
            else if 'A' <= c && c <= 'F'
                then 10 + ord c - ord 'A'
                else 10 + ord c - ord 'a'

parseHex :: String -> ParseMonad Integer
parseHex s = liftM listToInt $ sequence $ map (uncurry parseHexDigit) $ zip s [0..]
    where
        listToInt :: [Integer] -> Integer
        listToInt l = sum $ map (uncurry (*)) $ zip logs $ reverse l
        logs = 1:(map (*16) logs)

toString :: Integer -> ParseMonad String
toString = return . show

convert :: String -> String
convert s = str where
    (Right str) = tryParse s `catchError` printError
    tryParse s = do { n <- parseHex s; toString n }
    printError e = return $ niceMessage e
