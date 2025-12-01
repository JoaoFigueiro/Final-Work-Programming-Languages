module Interpreter where 

import Lexer 
import Parser 

isValue :: Expr -> Bool 
isValue BTrue  = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True 
isValue (Pair e1 e2) = isValue e1 && isValue e2
isValue _ = False

subst :: String -> Expr -> Expr -> Expr 
subst x s y@(Var v) = if x == v then 
                        s 
                      else 
                        y 
subst x s (Num n) = (Num n)
subst x s BTrue = BTrue 
subst x s BFalse = BFalse 
subst x s (Lam y tp t1) = Lam y tp (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2) 
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2) 
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2) 
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Paren t1) = Paren (subst x s t1)
-- Substituição em tuplas
subst x s (Pair e1 e2) = Pair (subst x s e1) (subst x s e2)
subst x s (Fst e) = Fst (subst x s e)
subst x s (Snd e) = Snd (subst x s e)


step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = let e2' = step e2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2 
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = let e2' = step e2
                             in Times (Num n1) e2' 
step (Times e1 e2) = Times (step e1) e2

step (And BFalse e2) = BFalse 
step (And BTrue e2) = e2 
step (And e1 e2) = And (step e1) e2 

step (Or BFalse e2) = e2
step (Or BTrue e2) = BTrue
step (Or e1 e2) = Or (step e1) e2

step (If BTrue e2 e3) = e2
step (If BFalse e2 e3) = e3
step (If e1 e2 e3) = If (step e1) e2 e3

step (Paren e1) = e1

step (App (Lam x tp e1) e2) = if (isValue e2) then 
                                subst x e2 e1 
                              else 
                                App (Lam x tp e1) (step e2)

step (App e1 e2) = App (step e1) e2

-- Avaliação de tuplas
step (Pair e1 e2) | not (isValue e1) = Pair (step e1) e2
                  | not (isValue e2) = Pair e1 (step e2)

step (Fst (Pair v1 v2)) | isValue v1 && isValue v2 = v1
step (Fst e) = Fst (step e)

step (Snd (Pair v1 v2)) | isValue v1 && isValue v2 = v2
step (Snd e) = Snd (step e)

step e = error ("Stuck at: " ++ show e)

eval :: Expr -> Expr
eval e = if isValue e then 
           e
         else 
           eval (step e)
