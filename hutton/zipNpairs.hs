zs :: [a] -> [b] -> [(a, b)] 
zs xs ys = [(xs!!i, ys!!i) | i <- [0..(min (length xs) (length ys))-1] ]
