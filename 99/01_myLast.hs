myLast :: [Int] -> Int
myLast xs = last xs
--myLast xs = head (reverse xs)
--myLast' xs = xs!!(length xs - 1)

myInit :: [Int] -> [Int]
myInit xs = reverse (tail (reverse xs)) 
--myInit xs = take (length xs - 1) xs
