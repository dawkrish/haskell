module Calc where

import ExprT
import Parser

--Exercise1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--Exercise2
evalString :: String -> Maybe Integer
evalString s = case (parseExp Lit Add Mul s) of
                Nothing  -> Nothing
                Just val -> Just (eval val)

--Exercise3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit n = Lit n
    add x y = Add x y
    mul x y = Mul x y

