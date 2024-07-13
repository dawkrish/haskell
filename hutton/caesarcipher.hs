import Data.Char

cipher :: [Char] -> Int -> [Char]
cipher xs shft = [conv c (shft `mod` 26) | c <- xs]

conv :: Char -> Int -> Char
conv c shft
    | isLower c = chr ((newCharIdx `mod` 26) + ord 'a')
    | otherwise = c 
     where
      newCharIdx = ord c - ord 'a' + shft

