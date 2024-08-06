{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fibs :: [Integer]
fibs = [0, 1] ++ [n | i <- [2 ..], let n = fibs !! (i - 1) + fibs !! (i - 2)]

data Stream a = Cons a (Stream a)

instance (Show a) => Show (Stream a) where
  show = show . take 20 . stream2List

stream2List :: Stream a -> [a]
stream2List (Cons x s) = x : stream2List s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons y s2) = Cons x (interleaveStreams s2 s1)

-- ruler :: Stream Integer
-- ruler = startRuler 0

-- startRuler :: Integer -> Stream Integer
-- startRuler y = interleaveStreams (streamRepeat y) (startRuler (y + 1))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons x xs) snd@(Cons y ys) = Cons (x * y) (streamMap (* x) ys + (xs * snd))

instance Fractional (Stream Integer) where
  (/) fst@(Cons x xs) snd@(Cons y ys) = q
    where
      q = Cons (div x y) (streamMap (`div` y) (xs - q * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)
