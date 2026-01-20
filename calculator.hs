import Control.Monad (forever)
import Data.Char (isDigit, isSpace)
import System.IO (hFlush, stdout)

data Token = Num Float | Op Char | ParOpen | ParClose deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | isSpace c = tokenize cs
    | c == '(' = ParOpen : tokenize cs
    | c == ')' = ParClose : tokenize cs
    | c `elem` "+-*/" = Op c : tokenize cs
    | isDigit c =
        let (num, rest) = span isDigit (c : cs)
         in Num (read num) : tokenize rest
    | otherwise = tokenize cs

parseFactor :: [Token] -> Maybe (Float, [Token])
parseFactor (Num x : rest) = Just (x, rest)
parseFactor (ParOpen : rest) = do
    (val, ParClose : rest') <- parseExpr rest
    Just (val, rest')
parseFactor _ = Nothing

parseTerm :: [Token] -> Maybe (Float, [Token])
parseTerm tokens = parseFactor tokens >>= uncurry go
  where
    go acc (Op '*' : rest) = parseFactor rest >>= \(y, next) -> go (acc * y) next
    go acc (Op '/' : rest) = parseFactor rest >>= \(y, next) -> go (acc / y) next
    go acc rest = Just (acc, rest)

parseExpr :: [Token] -> Maybe (Float, [Token])
parseExpr tokens = parseTerm tokens >>= uncurry go
  where
    go acc (Op '+' : rest) = parseTerm rest >>= \(y, next) -> go (acc + y) next
    go acc (Op '-' : rest) = parseTerm rest >>= \(y, next) -> go (acc - y) next
    go acc rest = Just (acc, rest)

expr :: [Token] -> Maybe Float
expr [] = Nothing
expr tokens = case parseExpr tokens of
    Just (val, []) -> Just val
    _ -> Nothing

main :: IO ()
main = forever $ do
    putStr "calc> " >> hFlush stdout
    input <- getLine
    case expr (tokenize input) of
        Nothing -> putStrLn "Error: Invalid expression"
        Just result -> print result
