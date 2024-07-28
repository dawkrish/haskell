import Data.List

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where
                        brak (Val n) = show n
                        brak e = "(" ++ show e ++ ")" 

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /=1

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <-eval r, valid o x y]

exprs :: [Int] -> [Expr]
exprs [x] = [Val x]
exprs xs = foldl (\acc (fst, snd) -> acc ++  [App op p q | p <- exprs fst, q <- exprs snd,  op <- [Add, Sub, Mul, Div]]) [] pairs 
    where pairs = [((take i xs),(drop i xs)) |  i <- [1..length xs - 1]] 

visual :: Expr -> String
visual (Val n) = show n
visual (App o l r) = "(" ++ visual l ++ show o ++ visual r ++ ")"

combos :: [Int] -> [[Int]]
combos xs= foldl (\acc x -> acc ++ permutations x) [] unique
    where unique = foldl (\acc x ->  acc ++ [[x]] ++ map (++[x]) acc)  [] xs

countdown :: [Int] -> Int -> [String]
countdown xs target= [show e | e <- es, eval e == [target]]
    where es = foldl (\acc x -> acc ++ (exprs x)) [] cs
          cs = combos xs
