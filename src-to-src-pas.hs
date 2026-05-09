{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Monad (when)
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Debug.Trace (trace, traceM, traceShowId)
import System.Environment (getArgs)
import Text.Show.Pretty (pPrint, ppShow)

-- <<GRAMMAR OF THE PASCAL PROG>>
-- comment: LCURLY COMMENT RCURLY
-- program: PROGRAM NAME SEMI comment? vars block DOT comment?
-- vars: (var | procedure)*
-- var: VAR (NAME COMMA?)* COLON TYPE SEMI comment?
-- procedure: PROCEDURE NAME LPAREN (VARNAME COLON TYPE COMMA?)* RPAREN SEMI vars block SEMI
-- block: BEGIN comment? ((assignment | expr) SEMI comment?)* END
-- assignment: ID ASSIGN (ID | expr) SEMI comment?
-- expr: term (PLUS | MINUS) term
-- term: factor (MUL | DIV) factor
-- factor: NUM | VARNAME | ID | LPAREN expr RPAREN

data NodeType = Comment | Var | Semi | Dot | Id | Num | BinOp | Block | Procedure | Program deriving (Show)

data ValueType = None | Real | Integer | Mul | Div | Plus | Minus | Assign deriving (Show, Eq)

data Node = Node
    { nodeType :: NodeType
    , name :: String
    , left :: Maybe Node
    , right :: Maybe Node
    , value :: String
    , valueType :: ValueType
    , children :: [Node]
    , vars :: [Node]
    , args :: [Node]
    , lvl :: Int
    }
    deriving (Show)

data SymVal = SNum Double | SString String deriving (Show)

type Symbol = String
data SymbolInfo = SymbolInfo
    { symType :: ValueType -- e.g., "Integer", "Boolean"
    , symVal :: Maybe SymVal -- or whatever value type you use
    , lvl :: Int
    }
    deriving (Show)

type SymbolTable = [Map.Map Symbol SymbolInfo]
data InterpreterState = InterpreterState
    { symTable :: SymbolTable
    , str :: String
    }
    deriving (Show)

-- Find a symbol by searching from local to global scopes
lookupSymbol :: Symbol -> SymbolTable -> Maybe SymbolInfo
lookupSymbol name [] = Nothing
lookupSymbol name (m : ms) =
    case Map.lookup name m of
        Just info -> Just info
        Nothing -> lookupSymbol name ms

-- Define the interpreter monad
type Interpreter a = State InterpreterState a

-- Add a symbol to the current (innermost) scope
addSymbol :: Symbol -> SymbolInfo -> Interpreter ()
addSymbol name info = modify $ \x -> x{symTable = (Map.insert name info (fromMaybe Map.empty $ listToMaybe x.symTable)) : drop 1 x.symTable}

-- Enter a new nested scope
pushScope :: Interpreter ()
pushScope = modify $ \x -> x{symTable = Map.empty : x.symTable}

-- Exit the current scope
popScope :: Interpreter ()
popScope = modify $ \x -> x{symTable = drop 1 x.symTable}

pStr :: String -> Interpreter ()
pStr s = modify $ \x -> x{str = s}

gStr :: Interpreter (String)
gStr = do
    s <- get
    return s.str

reservedKeywords = ["begin", "end"]

defNode :: Node
defNode = Node{nodeType = Comment, name = "", left = Nothing, right = Nothing, value = "", valueType = None, children = [], vars = [], args = [], lvl = 0}

comment :: Interpreter (Maybe Node)
comment = do
    str <- skipSpace'
    let (token, r1) = splitAt 1 $ skipSpace str
    case splitAt 1 str of
        ('{' : [], r1) -> do
            let (content, r2) = break (== '}') r1
            pStr $ drop 1 r2
            return $ Just defNode{nodeType = Comment, value = content}
        _ -> return Nothing

collectValuetype :: Interpreter (ValueType)
collectValuetype = do
    st <- get
    let (t, xs) = span isAlpha st.str
    pStr xs
    return $ case map toLower t of
        "real" -> Real
        "integer" -> Integer
        _ -> error "No type specified for a variable"

var :: Interpreter ([Node])
var = do
    str <- skipSpace'
    let (w, r1) = span isAlphaNum str
    let collectVarnames :: String -> [String] -> ([String], String)
        collectVarnames (':' : str) acc = (acc, dropWhile isSpace str)
        collectVarnames (',' : str) acc = collectVarnames str acc
        collectVarnames (' ' : str) acc = collectVarnames str acc
        collectVarnames str acc = let (name, rest) = span isAlpha str in collectVarnames rest (name : acc)
    let (varnames, r2) = collectVarnames r1 []
    pStr r2
    vartype <- collectValuetype
    let constructNode :: [String] -> Interpreter [Node]
        constructNode [] = return []
        constructNode (vname : vnames) = do
            st <- get
            let lvl = length st.symTable
            addSymbol vname SymbolInfo{symType = vartype, symVal = Nothing, lvl = lvl}
            nodes <- constructNode vnames
            return $ defNode{nodeType = Var, name = vname, valueType = vartype, lvl = lvl} : nodes
    semiStr <- gStr
    (hasSemi) <- semi
    (cmt) <- if hasSemi then comment else error $ "Semicolon is expected at" ++ semiStr
    if (map toLower w) == "var"
        then do
            nodes <- constructNode varnames
            return $ nodes ++ maybeToList cmt
        else do
            pStr str
            return []

vars :: Interpreter ([Node])
vars = do
    str <- skipSpace'
    let (kw, rest) = span isAlphaNum str
    case map toLower kw of
        "var" -> do
            v <- var
            ns <- vars
            return $ v ++ ns
        "procedure" -> do
            pushScope
            v <- procedure
            popScope
            ns <- vars
            return $ v ++ ns
        "begin" -> return []
        "" -> return []
        _ -> error $ "Unexpected syntax in vars block: " ++ take 20 str

factor :: Interpreter ([Node])
factor = do
    str <- skipSpace'
    case str of
        ('(' : xs) -> expr
        (')' : xs) -> do
            pStr xs
            return []
        (x : xs) | isAlpha x -> do
            let (id, rest) = span isAlphaNum str
            pStr rest
            interpreterState <- get
            let (lvl, valType) = case lookupSymbol id interpreterState.symTable of
                    Just symInfo -> (symInfo.lvl, symInfo.symType)
                    Nothing -> (0, None)
            return ([defNode{nodeType = Id, name = id, lvl = lvl, valueType = valType}])
        (x : xs) | isDigit x -> do
            let (num, rest) = span (\i -> isDigit x || x == '.') str
            pStr rest
            return ([defNode{nodeType = Num, value = num}])
        _ -> return []

term :: Interpreter ([Node])
term = do
    let getOp ('*' : ' ' : xs) = Just (Mul, xs)
        getOp ('/' : ' ' : xs) = Just (Div, xs)
        getOp _ = Nothing
    let go l = do
            str <- skipSpace'
            case getOp str of
                Just (op, rest) -> do
                    pStr rest
                    right <- factor
                    go [defNode{nodeType = BinOp, left = listToMaybe l, right = listToMaybe right, valueType = op}]
                Nothing -> do
                    return l
    _ <- skipSpace'
    left <- factor
    go left

expr :: Interpreter ([Node])
expr = do
    let getOp ('+' : ' ' : xs) = Just (Plus, xs)
        getOp ('-' : ' ' : xs) = Just (Minus, xs)
        getOp _ = Nothing
    let go l = do
            str <- skipSpace'
            case getOp str of
                Just (op, rest) -> do
                    pStr rest
                    right <- term
                    go [defNode{nodeType = BinOp, left = listToMaybe l, right = listToMaybe right, valueType = op}]
                Nothing -> do
                    return l
    _ <- skipSpace'
    left <- term
    go left

semi :: Interpreter (Bool)
semi = do
    str <- gStr
    let str' = skipSpace str
    if ";" `isPrefixOf` str'
        then do
            pStr $ drop 1 str'
            return True
        else do
            pStr str'
            return False

-- assignment: ID ASSIGN (ID | expr) SEMI comment?
assignment :: Interpreter ([Node])
assignment = do
    str <- skipSpace'
    let getId (x : xs)
            | isAlpha x = Just $ span isAlphaNum (x : xs)
            | otherwise = Nothing
        getId _ = Nothing
    let getAssign xs = if ":=" `isPrefixOf` xs then Just (drop 2 xs) else Nothing
    case getId str of
        Just (idName, r1) | Just r2 <- getAssign (skipSpace r1) -> do
            pStr r2
            interpreterState <- get
            let (lvl, valType) = case lookupSymbol idName interpreterState.symTable of
                    Just symInfo -> (symInfo.lvl, symInfo.symType)
                    Nothing -> (0, None)
            let idNode = defNode{nodeType = Id, name = idName, lvl = lvl, valueType = valType}
            exprNode <- expr
            r3 <- gStr
            hasSemi <- semi
            cmt <- comment
            let node = defNode{nodeType = BinOp, left = Just idNode, right = listToMaybe exprNode, valueType = Assign}
            if hasSemi
                then return $ node : maybeToList cmt
                else error $ "Unexpected syntax in assignment block at ::" ++ r3
        _ -> return []

getStatement :: Interpreter ([Node])
getStatement = do
    ns <- assignment
    case ns of
        [] -> do
            sstart <- gStr
            exprNode <- expr
            str <- gStr
            sem <- semi
            cmt <- if sem then comment else error $ "\n exprStart string:: " ++ sstart ++ "\nexprNode:: \n" ++ show exprNode ++ "\nUnexpected syntax in getStatement block at :: " ++ str
            return $ exprNode ++ maybeToList cmt
        _ -> return ns

statementList :: Interpreter ([Node])
statementList = do
    str <- skipSpace'
    case str of
        xs | "end" == (map toLower $ takeWhile isAlphaNum $ xs) -> return []
        "" -> return []
        _ -> do
            stmt <- getStatement
            stmts <- statementList
            return $ stmt ++ stmts

skipSpace :: String -> String
skipSpace = dropWhile isSpace

skipSpace' :: Interpreter (String)
skipSpace' = do
    str <- gStr
    let str' = skipSpace str
    pStr str'
    return str'

block :: Interpreter ([Node])
block = do
    str <- skipSpace'
    let pref = "begin" == (map toLower $ takeWhile isAlphaNum $ str)
    pStr $ drop 5 str
    cmt <- comment
    s <- skipSpace'
    stmtList <- statementList
    r1 <- gStr
    let postf = "end" == (map toLower $ takeWhile isAlphaNum $ skipSpace r1)
    pStr $ drop 3 (skipSpace r1)
    let node = defNode{nodeType = Block, children = [defNode{nodeType = Id, name = "begin"}] ++ (maybeToList cmt) ++ stmtList ++ [defNode{nodeType = Id, name = "end"}]}
    if pref && postf
        then return [node]
        else do
            pStr str
            return []

getArgument :: Interpreter (Node)
getArgument = do
    str <- skipSpace'
    let (name, rest) = span isAlphaNum str
    pStr $ dropWhile (\x -> isSpace x || x == ':') rest
    argType <- collectValuetype
    interpreterState <- get
    let lvl = length interpreterState.symTable
    addSymbol name SymbolInfo{lvl = lvl, symType = argType, symVal = Nothing}
    return defNode{nodeType = Var, name = name, valueType = argType, lvl = lvl}

getArguments :: Interpreter ([Node])
getArguments = do
    str <- skipSpace'
    case str of
        (' ' : xs) -> pStr xs >> getArguments
        (',' : xs) -> pStr xs >> getArguments
        ('(' : xs) -> pStr xs >> getArguments
        (')' : xs) -> pStr xs >> return []
        s | maybe False isAlpha $ listToMaybe str -> do
            n <- getArgument
            nx <- getArguments
            return $ n : nx
        _ -> return []

procedure :: Interpreter ([Node])
procedure = do
    str <- skipSpace'
    case str of
        ('p' : 'r' : 'o' : 'c' : 'e' : 'd' : 'u' : 'r' : 'e' : ' ' : xs) -> do
            let (procName, r1) = span isAlphaNum $ skipSpace xs
            interpreterState <- get
            let lvl = length interpreterState.symTable
            addSymbol procName SymbolInfo{symVal = Nothing, symType = None, lvl = lvl}
            pStr r1
            args <- getArguments
            hasSemi <- semi
            vs <- vars
            blk <- block
            r2 <- gStr
            hasSemi' <- semi
            cmt <- comment
            let pNode = if hasSemi' then defNode{nodeType = Procedure, name = procName, args = args, lvl = lvl - 1, vars = vs, children = blk ++ [defNode{nodeType = Semi}] ++ maybeToList cmt} else error $ "Semicolon expected at " ++ r2
            return $ [pNode]
        (' ' : xs) -> pStr xs >> procedure
        _ -> return []

-- program: PROGRAM NAME SEMI comment? vars block DOT comment?
program :: Interpreter ([Node])
program = do
    str <- skipSpace'
    pushScope
    case str of
        (' ' : xs) -> pStr xs >> program
        ('p' : 'r' : 'o' : 'g' : 'r' : 'a' : 'm' : ' ' : xs) -> do
            let (pName, r1) = break (\x -> isSpace x || x == ';') xs
            pStr r1
            hasSemi <- semi
            cmt <- comment
            vs <- if hasSemi then vars else error $ "Semicolon expected at " ++ r1
            blk <- block
            r2 <- gStr
            let (_, r3) = if "." `isPrefixOf` (skipSpace r2) then span (\x -> isSpace x || x == '.') r2 else error $ "DOT is expected at :: " ++ r2
            pStr r3
            cmt2 <- comment
            let n = defNode{nodeType = Program, name = pName, vars = maybeToList cmt ++ vs, children = blk ++ [defNode{nodeType = Dot}] ++ maybeToList cmt2}
            return [n]
        _ -> return []

indentLines :: Int -> String -> String
indentLines n = intercalate "\n" . map (replicate n ' ' ++) . lines

genBlock :: Node -> Int -> String
genBlock node nest = indentLines nest $ intercalate "" $ (map genBlockLine node.children)
  where
    genBlockLine n = case n.nodeType of
        BinOp | n.valueType == Assign, Just l <- n.left, Just r <- n.right -> "  " ++ l.name ++ show l.lvl ++ ":" ++ show l.valueType ++ " := " ++ genBin r ++ ";\n"
        Comment -> " {" ++ n.value ++ "}\n"
        Id -> if n.name `elem` reservedKeywords then n.name else n.name ++ show n.lvl ++ ":" ++ show n.valueType
        Semi -> ";"
        Dot -> "."
        Block -> genBlock n nest
        _ -> ""
    genBin n = case n.nodeType of
        BinOp | n.valueType == Minus, Just l <- n.left, Just r <- n.right -> genBin l ++ " - " ++ genBin r
        BinOp | n.valueType == Plus, Just l <- n.left, Just r <- n.right -> genBin l ++ " + " ++ genBin r
        BinOp | n.valueType == Mul, Just l <- n.left, Just r <- n.right -> genBin l ++ " * " ++ genBin r
        BinOp | n.valueType == Div, Just l <- n.left, Just r <- n.right -> genBin l ++ " / " ++ genBin r
        Num -> n.value
        Id -> n.name ++ show n.lvl ++ ":" ++ show n.valueType
        _ -> ""

genArgs :: [Node] -> String
genArgs nodes = intercalate ", " $ map genArg nodes
  where
    genArg node = case node.nodeType of
        Var -> node.name ++ show node.lvl ++ ":" ++ show node.valueType
        _ -> ""

genProcedure :: Node -> Int -> String
genProcedure node nest =
    indentLines nest $
        intercalate
            "\n"
            [ "procedure " ++ node.name ++ show node.lvl ++ "(" ++ (genArgs node.args) ++ ");"
            , genVars node.vars nest
            , genBlock node (nest - 1)
            ]

genVar :: Node -> Int -> String
genVar node nest = indentLines nest $ intercalate "\n" $ ["var " ++ node.name ++ show node.lvl ++ ": " ++ show node.valueType ++ ";"]

genVars :: [Node] -> Int -> String
genVars nodes nest = indentLines nest $ intercalate "\n" $ map mapVar nodes
  where
    mapVar node = case node.nodeType of
        Program -> genProgram node nest
        Procedure -> genProcedure node nest
        Var -> genVar node nest
        _ -> ""

genProgram :: Node -> Int -> String
genProgram node nest =
    indentLines nest $
        intercalate
            "\n"
            [ "\nprogram " ++ node.name ++ show node.lvl ++ ";"
            , genVars node.vars (nest + 1)
            , genBlock node nest ++ "\n\n"
            ]

genSrc :: [Node] -> String
genSrc nodes = unwords $ map genNode nodes
  where
    genNode n = case n.nodeType of
        Program -> genProgram n 0
        _ -> ""

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath : _) -> do
            content <- readFile filePath
            let (n, state) = runState program InterpreterState{symTable = [], str = content}
            putStrLn "Nodes :: "
            pPrint n
            putStrLn $ ppShow $ "Rest of string :: " ++ state.str
            putStrLn "-------------------------------------------"
            putStr $ genSrc n
        [] -> putStrLn "Usage: program <filename>"
