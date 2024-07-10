halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) 
            where n = length xs `div` 2
--halve xs = splitAt (length xs ‘div‘ 2) xs


third :: [a] -> Maybe a 
third xs = if length xs > 2 
            then Just (head(tail(tail xs)))
            else Nothing
--third xs = xs !! 2
--third (_:_:x:_) = x


safetail :: [a] -> [a]
--safetail xs =  if null xs then [] else tail xs
--safetail xs | null xs = []
 --           | otherwise = tail xs
safetail [] = []
safetail (_:xs) = xs


myOR :: Bool -> Bool -> Bool
False `myOR` b = b
True `myOR` _ = True

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

add :: Int -> Int -> Int
add = \x -> (\y -> x+ y)
