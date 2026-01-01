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

eval :: [Token] -> Maybe Float
eval [] = Nothing
eval (Num n:rest) = Just $ foldl apply n (pairs rest)
  where
    pairs (Op op : Num val : ts) = (op, val) : pairs ts
    pairs _ = []
    apply acc ('+', val) = acc + val
    apply acc ('-', val) = acc - val
    apply acc ('*', val) = acc * val
    apply acc ('/', val) = acc / val
    apply acc _ = acc
eval _ = Nothing

main :: IO ()
main = forever $ do
  putStr "calc> " >> hFlush stdout
  input <- getLine
  case eval (tokenize input) of
    Nothing -> putStrLn "Error: Invalid expression"
    Just result -> print result
