module Lambda.Stmt(Stmt(..), Assign, Scope) where

data Stmt = V String
            | L String Stmt
            | A Stmt Stmt
            deriving (Eq)
type Assign = (String, Stmt)
type Scope = [Assign]

instance Show Stmt where
  show (V a) = a
  show (L v e) = "λ" ++ v ++ "." ++ (show e)
  show (A l r@(A _ _)) = (show l) ++" (" ++ (show r) ++")"
  show (A l r) = (show l) ++ " " ++ (show r)
