-- Zip function
zs :: [a] -> [b] -> [(a, b)]
zs xs ys = [(xs!!i, ys!!i) | i <- [0..min (length xs) (length ys)-1] ]

sumNsqr :: Int -> Int
sumNsqr n = sum [x^2 | x <- [1..n]]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square x = [(x,y) | (x,y) <- grid x x, x/=y]

replicate' :: Int -> a -> [a]
replicate' n x = [x| _ <-[1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <-[1..n], sum (factors x) == 2*x ]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys
    | length xs == length ys = sum [xs!!i * ys!!i | i <- [0..length xs - 1]]
    | otherwise = error "Lists must have the same length for scalar product"
--scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
