module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement =
      Assignment String Expr.T        
    | If Expr.T Statement Statement   
    | Skip                            
    | Begin [Statement]               
    | While Expr.T Statement          
    | Read String                     
    | Write Expr.T 
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


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  -- Use the ! operator to find the correct parser
  parse = skip ! assignment ! if' ! begin ! while ! read' ! write

  toString = error "Statement.toString not implemented"
