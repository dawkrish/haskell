dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> 10*acc+x) 0

bin2int :: [Int] -> Int
bin2int = foldl (\acc x -> 2*acc+x) 0

curry' :: ((a, b) -> c) -> (a ->b -> c)
curry' f = \x y -> f(x,y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x,y) -> f x y

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = foldl (\acc (i, x) ->
  if even i then acc ++ [f x] else acc ++ [g x]) [] (zip [0..] xs)
