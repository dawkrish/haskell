{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

import ExprT
import Parser
import StackVM

--Exercise1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

--Exercise2
evalString :: String -> Maybe Integer
evalString s = case parseExp Lit ExprT.Add ExprT.Mul s of
                Nothing  -> Nothing
                Just val -> Just (eval val)

--Exercise3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Add

reify :: ExprT ->  ExprT
reify = id

--Exercise4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<=0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax l1) (MinMax l2) = MinMax (max l1 l2)
    mul(MinMax l1) (MinMax l2) = MinMax (min l1 l2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7 :: Integer)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7 :: Integer)

--Exercise5
instance Expr Program where
    lit x = [PushI x]
    add xs ys = xs ++ ys ++ [StackVM.Add]
    mul xs ys = xs ++ ys ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
