main :: IO()
main = do
  putStr "Player 1, how many stars to remove: "
  x <- getChar
  putStrLn ""
  putStr "Player 2, how many stars to remove: "
  getChar
  y <- getChar
  return ()
  --getLine
  --putStrLn [x]
