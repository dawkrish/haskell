validate :: Int -> Bool
validate n = (sumDigits . doubleEveryOther. toDigits) n `mod` 10 == 0

sumDigits :: [Int] ->Int
sumDigits xs = sum (map sumDigitsHelper xs)

sumDigitsHelper :: Int -> Int
sumDigitsHelper n = if n < 10
                        then n
                        else (n `div` 10) + (n `mod` 10)

toDigits :: Int -> [Int]
toDigits n = if n >=  10
                then toDigits (n `div` 10)++[n `mod` 10]
                else [n]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs = reverse (doubleEveryOtherHelper (reverse xs) 0)

doubleEveryOtherHelper :: [Int] -> Int -> [Int]
doubleEveryOtherHelper xs i = if i /= length xs
                then (
                    if even i
                        then (xs!!i) : doubleEveryOtherHelper xs (i+1)
                        else (xs!!i * 2) : doubleEveryOtherHelper  xs (i+1)
                )
                else []
