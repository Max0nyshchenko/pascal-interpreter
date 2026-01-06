import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Data.Char (isDigit, isSpace)

data Token = Num Float | Op Char deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs) 
  | isSpace c = tokenize cs
  | c `elem` "+-*/" = Op c : tokenize cs
  | isDigit c = let (num, rest) = span isDigit (c:cs)
                in Num (read num) : tokenize rest
  | otherwise = tokenize cs

expr :: [Token] -> Maybe Float
expr [] = Nothing
expr tokens = Just $ sumUp $ mulDiv tokens []
  where
    mulDiv (Num x: Op '*' : Num y : rest) acc = mulDiv (Num (x * y) : rest) acc
    mulDiv (Num x: Op '/' : Num y : rest) acc = mulDiv (Num (x / y) : rest) acc
    mulDiv (x:rest) acc = mulDiv rest (acc ++ [x])
    mulDiv [] acc = acc
    sumUp (Num x : Op '+' : Num y : rest) = sumUp (Num (x + y) : rest)
    sumUp (Num x : Op '-' : Num y : rest) = sumUp (Num (x - y) : rest)
    sumUp [Num x] = x
    sumUp _ = 0

main :: IO ()
main = forever $ do
  putStr "calc> " >> hFlush stdout
  input <- getLine
  case expr (tokenize input) of
    Nothing -> putStrLn "Error: Invalid expression"
    Just result -> print result
