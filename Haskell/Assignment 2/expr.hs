-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

-- Ludvig Järvi, ludjrv-1 --

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)

    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const (10*n+(ord y - 48)), ys)

    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)


    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys

    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys

    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
-- Task 1:
unparse (App fn e) = fn ++ "(" ++ unparse e ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
-- Task 1:
eval (App "sin" e) env = sin (eval e env)
eval (App "cos" e) env = cos (eval e env)
eval (App "log" e) env = log (eval e env)
eval (App "exp" e) env = exp (eval e env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) = Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) = Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
-- Task 1:
diff v (App "sin" e) = Op "*" (App "cos" e) (diff v e)
diff v (App "cos" e) = Op "*" (Op "*" (Const (-1)) (App "sin" e)) (diff v e)
diff v (App "log" e) = Op "/" (diff v e) e
diff v (App "exp" e) = Op "*" (App "exp" e) (diff v e)
diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
-- Task 1:
simplify (App fn e) =
  let es = simplify e in
    case (fn, es) of
      ("sin", Const 0) -> Const 0 -- sin(0) = 0
      ("cos", Const 0) -> Const 1 -- cos(0) = 1
      ("log", Const 1) -> Const 0 -- log(1) = 0
      ("exp", Const 0) -> Const 1 -- exp(0) = 1
      (fn, e) -> App fn (simplify e) -- otherwise, just simplify the argument


-- The function can be evaluated by applying eval to body in a context where var is bound to its value
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, Var var) = (\x -> eval (simplify body) [(var, x)])

-- Solve equation using Newton-Raphson's method
findzero :: String -> String -> Float -> Float
findzero name body = nrStep f f'
  where -- Calculate the function and its derivative only once
    f = mkfun (parse body, Var name)
    f' = mkfun (simplify (diff (Var name) (parse body)), Var name)

-- Newton-Raphson step
nrStep :: (Float -> Float) -> (Float -> Float) -> Float -> Float
nrStep f f' x0
  | abs (x1 - x0) <= 0.0001 = x0
  | otherwise = nrStep f f' x1
  where
    x1 = x0 - f x0 / f' x0

main :: IO()
main = do
  -- Test parse
  putStrLn "\n-- Test: parse"
  print (parse "10")
  print (parse "x")
  print (parse "10+x")
  print (parse "1+2*(3-4/5)")
  print (unparse (parse "sqrt(1+sin(x))"))

  -- Task 1: Add support for sin, cos, log, exp
  putStrLn "\n-- Task 1: Add support for sin, cos, log, exp"
  print (unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))) -- should print the derivative: exp(sin(2*x))*cos(2*x)*2

  -- Task 2: Add mkfun
  putStrLn "\n-- Task 2: mkfun"
  print(mkfun (parse "x*x+2", Var "x") 3.0) -- should evaluate to 11.0

  -- Task 3: Add findzero
  putStrLn "\n-- Task 2: findzero"
  print(findzero "x" "x*x*x+x-1" 1.0) -- should evaluate to 0.68232775
  print(findzero "y" "cos(y)*sin(y)" 2.0) -- should evaluate to 1.5707964
  print(findzero "z" "z*z*z+1337" 5.0) -- should evaluate to -11.016504