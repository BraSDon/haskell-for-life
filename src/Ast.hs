module Ast where

data Exp t = 
    Var t | 
    Add (Exp t) (Exp t) | 
    Const Integer |
    Less (Exp t) (Exp t) |
    And (Exp t) (Exp t) |
    Not (Exp t) |
    If (Exp t) (Exp t) (Exp t)

type Env t = t -> Integer

eval :: Env t -> Exp t -> Integer
eval f (Var v) = f v
eval f (Add exp1 exp2) = eval f exp1 + eval f exp2
eval f (Const c) = c
eval f (Less e1 e2) = if eval f e1 < eval f e2 then 1 else 0
eval f (And e1 e2) = if eval f e1 + eval f e2 == 2 then 1 else 0
eval f (Not e) = if eval f e == 0 then 1 else 0
eval f (If i t e) = if eval f e == 1 then eval f t else eval f e

instance Show a => Show (Exp a) where
  show (Var v) = show v
  show (Const c) = show c
  show (Add e1 e2) = show "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Less e1 e2) = show "(" ++ show e1 ++ "<" ++ show e2 ++ ")"
  show (And e1 e2) = show "(" ++ show e1 ++ "&" ++ show e2 ++ ")"
  show (Not e) = show "!(" ++ show e ++ ")"
  show (If i t e) = show "(" ++ show i ++ "?" ++ show t ++ ":" ++ show e ++ ")"
