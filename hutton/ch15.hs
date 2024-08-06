fibs :: [Integer]
fibs = [0, 1] ++ [n | i <- [2 ..], let n = fibs !! (i - 1) + fibs !! (i - 2)]

initial = 1.0
dist = 0.00001

sqr :: Double -> Double
sqr n = last xs
  where xs = n : [x| i <- [1..], let x = next n (xs !! (i-1)), (xs !! (i-1)) - x >= dist]

next :: Double -> Double -> Double
next n a = (a + n / a) / 2
