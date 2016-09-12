module Syntax where


data Expr
  = Var String
  | App Expr Expr
  | Lam String Expr
  | Lit Lit
  deriving (Eq, Show)

data Lit = LInt Int | LBool Bool deriving (Eq, Show)
