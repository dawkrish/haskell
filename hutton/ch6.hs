insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
    | n > x = x : insert n xs
    | otherwise = n:x:xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x : myInit xs

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n| n > 0 = n + sumdown (n-1)

{-
 pow 2 3 = 2 * pow 2 2
         = 2 * 2 * pow 2 1
         = 2 * 2 * 2 * pow 2 0
         = 2 * 2 * 2 * 1
 -}
pow :: Int -> Int -> Int
pow 0 b = 0 
pow a 0 = 1 
pow a b = a * pow a (b-1)

{-
 euclid 27 6 = euclid 6 3    {27 `mod` 6}
             = euclid 3 0    {6 `mod` 3}
             = 3

euclid 6 27 = euclid 27 6   {6 `mod 27}
            = 3    {calculated above}
 -}
euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (a `mod` b)


myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (False:_) = False
myAnd (_:xs) = myAnd xs 

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++  myConcat xss

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs)
    | e == x = True
    | otherwise = myElem e xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    |x < y = x : merge xs (y:ys)
    |otherwise = y : merge (x:xs) ys

{-
 msort [4,5,3,2,1] = merge (msort [4,5]) (msort [3,2,1])
                   = merge (merge (msort [4]) msort([5])) (merge (msort[3]) msort([2,1]))
                   = ""    (merge 4 5) (merge 3 (merge msort([2]) msort([1])))
                   = ""    [4,5] (merge 3 merge [2] [1])
                   = ""    [4,5] (merge 3 [1,2])
                   = merge    [4,5] [1,2,3]
                   = [1,2,3,4,5]
 -}
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where mid = length xs `div` 2
          (left, right) = splitAt mid xs
