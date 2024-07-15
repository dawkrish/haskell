module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = [skip xs i| i <-[1..length xs]]

-- Retruns elements from nth position with jumps of n
-- skip "abcd" 2 = "bd" {starts from 2nd element and skips 2 forward}
skip :: [a] -> Int -> [a]
skip xs n = [xs!!i | i <- [n-1,n-1+n..length xs - 1]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Int] -> String
histogram xs = graph hists 0 nrows ++ "==========\n" ++ "0123456789\n"
  where
    hists = freq xs
    nrows = maximum hists

graph :: [Int] -> Int -> Int -> String
graph hists start end
  | start == end = ""
  | otherwise = graph (map (subtract 1) hists) (start+1) end ++ row
    where row = buildRow hists

buildRow :: [Int] -> String
buildRow [] = "\n"
buildRow (x:xs)
  | x > 0 = "|" ++ buildRow xs
  | otherwise = " " ++ buildRow xs

freq :: [Int] ->[Int]
freq xs = [length (filter (==uniq) xs) | uniq<-[0..9]]
