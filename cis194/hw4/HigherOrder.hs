--Exercise1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/=1)
      . iterate (\n -> if even n then n `div` 2 else 3*n+1)

--Exercise2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h left root right)
  | h1 < h2   = Node h (insert x left) root right
  | h1 > h2   = Node h left root (insert x right)
  | otherwise = Node (h3+1) left' root right
  where h1 = height left
        h2 = height right
        h3 = height left'
        left' = insert x left

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

{-My old solution without foldr and a different insertion order-}
--foldTree (x:xs) = Node height (foldTree left) x (foldTree right)
--    where (left, right) = splitAt half xs
--          half = length xs `div` 2
--          height = floor (logBase 2 (int2Float(length xs)))

--Exercise3
xor :: [Bool] -> Bool
xor = foldr (/=) True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x->2*x+1) (filter f [1..n])
    where f x = x `notElem` takeWhile (<=n) [i+j+2*i*j| i <- [1..], j <-[1..], i <= j]

