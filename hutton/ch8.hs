
-- Abstract Machine
data Expr = Val Int | Add Expr Expr
data Op = EVAL Expr|  ADD Int
type Cont =  [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c) 

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)

value :: Expr -> Int
value e = eval e []

--HW
--Q3
data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: Tree a-> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
