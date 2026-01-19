import Control.Monad (forever)
import Data.Char (isDigit, isSpace)
import System.IO (hFlush, stdout)

data Token = Num Float | Op Char deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | isSpace c = tokenize cs
    | c `elem` "+-*/" = Op c : tokenize cs
    | isDigit c =
        let (num, rest) = span isDigit (c : cs)
         in Num (read num) : tokenize rest
    | otherwise = tokenize cs

parseFactor :: [Token] -> Maybe (Float, [Token])
parseFactor (Num x : rest) = Just (x, rest)
parseFactor (Op '(' : rest) = do
    (val, Op ')' : rest') <- parseExpr rest
    Just (val, rest')
parseFactor _ = Nothing

parseTerm :: [Token] -> Maybe (Float, [Token])
parseTerm tokens = do
    (x, rest) <- parseFactor tokens
    go x rest
  where
    go acc (Op '*' : rest) = do
        (y, rest') <- parseFactor rest
        go (acc * y) rest'
    go acc (Op '/' : rest) = do
        (y, rest') <- parseFactor rest
        go (acc / y) rest'
    go acc rest = Just (acc, rest)

parseExpr :: [Token] -> Maybe (Float, [Token])
parseExpr tokens = do
    (x, rest) <- parseTerm tokens
    go x rest
  where
    go acc (Op '+' : rest) = do
        (y, rest') <- parseTerm rest
        go (acc + y) rest'
    go acc (Op '-' : rest) = do
        (y, rest') <- parseTerm rest
        go (acc - y) rest'
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
