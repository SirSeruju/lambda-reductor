module Lambda where

type Name = Char

data Expr 
  = Var Name
  | App Expr Expr
  | Abs Name Expr

instance Show Expr where
  show (Var name) = [name]
  show (App e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (Abs name body) = "(" ++ [name] ++ "." ++ (show body) ++ ")"


-- NF   ::= \x.NF | NANF
-- NANF ::= v     | NANF NF
-- NA   ::= v     | N M
reduction :: Expr -> Expr
-- if   NA -> NA`
-- then NA N -> NA` N
-- Just check left apperand
reduction (App a@(App _ _) expr) = reduction $ App (reduction a) expr

-- if   N -> N'
-- then NANF N -> NANF N'
-- If left apperand is NF then check right
reduction (App expr a@(App _ _)) = reduction $ App expr (reduction a)

-- if   N -> N'
-- then \x.N -> \x.N'
-- If body of abstraction may reduce, do it
reduction (Abs name a@(App _ _)) = Abs name (reduction a)

-- (\x.M) N -> M[x:=N]
-- Beta-reduction
reduction (App (Abs name body) expr) = reduction $ red body expr
  where
    red v@(Var n) e
      | n == name = e
      | otherwise = v
    red a@(Abs n b) e
      | n == name = a
      | otherwise = Abs n (red b e)
    red (App e1 e2) e = App (red e1 e) (red e2 e)

-- Else just return expr
reduction x = x
