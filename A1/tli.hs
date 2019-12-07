-- Professor's implementation
-- a guide to help us since we're trash at coding in Haskell lmao
import System.Environment 
import Control.Monad  
import Data.Char
import Data.List
import Data.List.Split
import System.IO
import Debug.Trace

type SymTable = [(String,Float)]

data Expr = Constant Float | Var String | Str String |
     LE_ Expr Expr |
     GE_ Expr Expr |
     LT_ Expr Expr |
     GT_ Expr Expr |
     EQ_ Expr Expr |
     NEQ_ Expr Expr |
     Minus Expr Expr |
     Times Expr Expr |
     Div Expr Expr |
     ExprError String |
     Plus Expr Expr deriving (Show) 

data Stmt =
     Let String Expr |
     If Expr String |
     Input String |
     Error String |
     Print [Expr] deriving (Show) 

lookupVar:: String -> SymTable -> Float
lookupVar name [] = 0
lookupVar name ((id,v):rest) = if (id == name) then v else lookupVar name rest

eval:: Expr ->SymTable -> Float
eval (Var v) env = lookupVar v env
eval (Constant v) _ = v
eval (Plus e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env = (eval e1 env) / (eval e2 env)
eval (LE_ e1 e2) env = if (eval e1 env) <= (eval e2 env) then 1 else 0
eval (GE_ e1 e2) env = if (eval e1 env) >= (eval e2 env) then 1 else 0
eval (LT_ e1 e2) env = if (eval e1 env) < (eval e2 env) then 1 else 0
eval (GT_ e1 e2) env = if (eval e1 env) > (eval e2 env) then 1 else 0
eval (EQ_ e1 e2) env = if (eval e1 env) == (eval e2 env) then 1 else 0

-- "print" a list of Expr to a String. SymTable is needed to get values of variables.
printList:: [Expr] -> SymTable -> String
printList [] _  = "\n"
printList ((Str s):rest) env = s++" "++(printList rest env)
printList (e:rest) env = (show (eval e env))++" " ++ (printList rest env)

-- Stmt, SymTable, progCounter, input, (SymTable', input', output, progCounter)
perform:: Stmt -> SymTable -> Float -> [String] -> (SymTable, [String], Maybe String, Float)
perform (Print elist) env lineNum input = (env, input, Just (printList elist env), lineNum+1)
perform (Let id e) env lineNum input = ((id,(eval e env)):env, input, Nothing, lineNum+1)
perform (If boolexpr label) env lineNum input = 
	if (eval boolexpr env) /= 0 then (env, input, Nothing, lookupVar (label++":") env)
	else (env, input, Nothing, lineNum+1)
perform (Input id) env lineNum (nxt:rest) = ((id, read nxt):env, rest, Nothing, lineNum+1)
perform (Error msg) env lineNum input = (env, input, Just ("Error on line " ++ (show lineNum) ++ "\n"++msg++"\n"), -1)

run :: [Stmt] -> Float -> [(String, Float)] -> [String] -> IO ()
run stmtList lineNum env input = 
    if lineNum >= 1 && lineNum <= fromIntegral (length stmtList) then 
        let (env1, input1, output1, lineNext) = perform (stmtList !! round (lineNum-1)) env lineNum input in 
	    case output1 of Nothing ->  run stmtList lineNext env1 input1
	             	    (Just line) -> do putStr line; run stmtList lineNext env1 input1
    else return ()

isLabel:: String -> Bool
isLabel lbl = if isSuffixOf ":" lbl then True else False

-- takes a list of tokens, a ST, a lineNum  and returns the parsed statement and the ST updated with a new label if one is found
parseLine :: [String] -> SymTable -> Float -> (Stmt, SymTable)
parseLine (first:rest) env lineNum =
	  if (isLabel first) then (parseStmt (head rest) (tail rest), (first,lineNum):env)
	  	  else (parseStmt first rest, env)

-- takes a list of tokens as strings and returns the parsed expression
parseExpr :: [String] -> Expr
parseExpr (e1:"+":e2:[]) = let pe1 = parseExpr [e1]; pe2 = parseExpr [e2] in
	  case (pe1,pe2) of ((ExprError _),_) -> error (unwords (e1:"+":e2:[]))
	       		    (_, (ExprError _)) -> error (unwords (e1:"+":e2:[]))
			    _ -> Plus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"-":e2:[]) = Minus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"*":e2:[]) = Times (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"/":e2:[]) = Div (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<":e2:[]) = LT_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:">":e2:[]) = GT_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<=":e2:[]) = LE_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:">=":e2:[]) = GE_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"==":e2:[]) = EQ_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"!=":e2:[]) = NEQ_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (x:rest) = if (isAlpha (head x) && rest == []) then (Var x) 
	      	else if ( head x == '"') && (last x == '"') then (Str  (init (tail x))) 
		else if ( head x == '"') && ((last (last rest)) == '"') 
		          then let longString = unwords (x:rest) in (Str (init (tail longString)))
  	        else if ( head x == '"') then (error (unwords (x:rest)))
		else if (isDigit (head x) && rest == []) then (Constant (read x))
		else (error (unwords (x:rest)))

-- separate an if-statment into the stuff between if and goto and the label which comes after goto
splitGoTo :: [String] -> ([String], [Char])
splitGoTo (x:"goto":lbl) = ([x],head lbl)
splitGoTo (x:more) = let (elist, lbl1) = splitGoTo more in (x:elist,lbl1)

parseExprList:: [String] -> [Expr]
parseExprList [] = []
parseExprList list = 
	      let oneString = foldl (\a b -> a ++ " " ++ b) "" list
	          byComma = splitOn "," oneString
		  exprStringList = map words byComma
	      in  map parseExpr exprStringList

parseStmt:: String -> [String] -> Stmt
parseStmt "let" (v:"=":expr) = let pe = parseExpr expr in case pe of (ExprError msg) -> Error msg
	  		       	      		       	       	     _ -> Let v (parseExpr expr)
parseStmt "print" exprList = Print (parseExprList exprList)
parseStmt "if" rest = let (elist,lbl) = splitGoTo rest in If (parseExpr elist)  lbl
parseStmt "input" [varName] = Input varName
parseStmt first rest = error (unwords (first:rest))

-- list of tokenized lines, ST, lineNum, -> (list of parsed Stmt, ST with labels)
parseAll:: [[String]] -> SymTable -> Float -> ([Stmt], SymTable)
parseAll [] env _ = ([], env)
parseAll (first:rest) env lineNum = 
	 let (stmt, env1) = parseLine first env lineNum 
	     (theRest, env2) = parseAll rest env1 (lineNum+1)
	 in (stmt:theRest, env2)
main = do
     args <- getArgs
     pfile <- openFile (head args) ReadMode
     contents <- hGetContents pfile
     userInput <- getContents
     let (stmtList, env) = parseAll (map words (lines contents)) [] 1 in run stmtList 1 env (words userInput) 
     hClose pfile

-- just for testing
prg1 = [Let "x" (Constant 1), Let "y" (Constant 2), Print [(Plus (Var "x") (Var "y"))]]
el1 = ["\"abc\",","x + y,","z"]
sample = ["input foo","let x = 1", "two: let y = 2", "let z = x + y", "if z != 3 goto two","print z"]

-- given list of list of tokens, a ST, and return the list of parsed Stmts and ST storing mapping of labels to line numbers
parseTest :: [[String]] -> SymTable -> ([Stmt], SymTable)
parseTest prg st = parseAll prg st 1.0

