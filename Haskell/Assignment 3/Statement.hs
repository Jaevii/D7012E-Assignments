module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

-- program ::= statements
-- statement ::= variable ':=' expr ';'
--    | 'skip' ';'
--    | 'begin' statements 'end'
--    | 'if' expr 'then' statement 'else' statement
--    | 'while' expr 'do' statement
--    | 'read' variable ';'
--    | 'write' expr ';'
-- statements ::= {statement}
-- variable ::= letter {letter}

data Statement =
      Assignment String Expr.T        
    | If Expr.T Statement Statement   
    | Skip                            
    | Begin [Statement]               
    | While Expr.T Statement          
    | Read String                     
    | Write Expr.T
    | Repeat Statement Expr.T
    deriving Show

-- Assignment
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- If
if' = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, then'), else') = If e then' else'

-- Skip
skip = accept "skip" #- require ";" >-> const Skip

-- Begin
begin = accept "begin" -# iter parse #- require "end" >-> Begin

-- While
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

-- Read
read' = accept "read" -# word #- require ";" >-> Read

-- Write
write = accept "write" -# Expr.parse #- require ";" >-> Write

-- Repeat
repeat' = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (s, e) = Repeat s e


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

-- Base case
exec [] _ _ = []

exec (Assignment v e: stmts) dict input =
    exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict) > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Skip: stmts) dict input = 
    exec stmts dict input

exec (Begin s: stmts) dict input = 
    exec (s++stmts) dict input

exec (While e s: stmts) dict input = 
    if (Expr.value e dict) > 0
    then exec (s: While e s: stmts) dict input
    else exec stmts dict input

exec (Read v: stmts) dict input =
    exec stmts (Dictionary.insert (v, head input) dict) (tail input)

exec (Write e: stmts) dict input = 
    Expr.value e dict : exec stmts dict input

exec (Repeat s e: stmts) dict input =
  exec (s : control : stmts) dict input
  where
    control = If e Skip (Repeat s e)

instance Parse Statement where
  -- Use the ! operator to find the correct parser
  parse = skip ! assignment ! if' ! begin ! while ! read' ! write ! repeat'

  toString :: Statement -> String
  toString (Assignment v e) = v ++ " := " ++ Expr.toString e ++ ";\n"
  toString (If cond thenStmts elseStmts) = "if " ++ Expr.toString cond ++ " then\n" ++ toString thenStmts ++ "else\n" ++ toString elseStmts
  toString (Skip) = "skip;\n"
  toString (Begin s) = "begin\n" ++ concatMap toString s ++ "end\n"
  toString (While e s) = "while " ++ Expr.toString e ++ " do\n" ++ toString s
  toString (Read v) = "read " ++ v ++ ";\n"
  toString (Write e) = "write " ++ Expr.toString e ++ ";\n"
  toString (Repeat s e) = "repeat\n" ++ toString s ++ "until " ++ Expr.toString e ++ ";\n"
