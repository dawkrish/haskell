import Data.Char

main :: IO ()
main = do
  play [5, 4, 3, 2, 1] 1

next :: Int -> Int
next 1 = 2
next 2 = 1

play :: [Int] -> Int -> IO ()
play board chance=
  if finish board
    then do
      putStrLn ("Player" ++ show (next chance) ++ " won!")
      return ()
    else do
      putStr (visualize board)
      putStrLn ("chance of Player" ++ show chance)
      r <- validRow
      n <- validStars board r
      play (update board r n) (next chance)

validRow :: IO Int
validRow = do
  putStr "select row number: "
  r <- getChar
  putStrLn ""
  if not (isDigit r)
    then do
      putStrLn "Its not a digit :("
      validRow
    else
      if digitToInt r >= 1 && digitToInt r <= 5
        then return (digitToInt r)
        else do
          putStrLn "Not a valid row number :( "
          validRow

validStars :: [Int] -> Int -> IO Int
validStars board r = do
  putStr "select number of stars: "
  n <- getChar
  putStrLn ""
  if not (isDigit n)
    then do
      putStrLn "Its not a digit :("
      validStars board r
    else
      if digitToInt n <= board !! (r - 1)
        then return (digitToInt n)
        else do
          putStrLn "Not Enough Stars"
          validStars board r

visualize :: [Int] -> String
visualize xs = concat [show (i + 1) ++ ": " ++ foo (xs !! i) ++ "\n" | i <- [0 .. length xs - 1]]
  where
    foo n = concat (replicate n "* ")

finish :: [Int] -> Bool
finish = all (== 0)

update :: [Int] -> Int -> Int -> [Int]
update xs r n = [if (r - 1) == i then (xs !! i) - n else xs !! i | i <- [0 .. length xs - 1]]
