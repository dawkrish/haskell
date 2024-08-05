main :: IO()
main = do
  play [5,4,3,2,1]
  --getLine
  --putStrLn [x]

play :: [Int] -> IO()
play board = do
  putStr (visualize board)
  putStr "select row number: "
  r <- getChar
  putStrLn ""
  putStr "select no. of stars: "
  n <- getChar
  putStrLn ""


visualize :: [Int] -> String
visualize xs = concat [show (i+1) ++ ": " ++ foo (xs !! i)  ++ "\n" | i <- [0..length xs - 1]]
  where foo n = concat (replicate n "* ")

update :: [Int] -> Int -> Int -> [Int]
update xs r n = [if (r - 1) == i then (xs!!i) - n else xs !! i | i <- [0..length xs - 1] ]
