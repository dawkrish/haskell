module LogAnalysis where

import Log


parse :: String -> [LogMessage]
parse file = [parseMessage line| line <- lines file]

parseMessage :: String -> LogMessage
parseMessage s
  | firstWord == "I" = LogMessage Info (intAt 1) (dropNwords 2)
  | firstWord == "W" = LogMessage Warning (intAt 1) (dropNwords 2)
  | firstWord == "E" = LogMessage (Error (intAt 1)) (intAt 2) (dropNwords 3)
  | otherwise = Unknown s
  where
  splitWords = words s
  firstWord = head splitWords
  intAt x = read (splitWords !! x) :: Int
  dropNwords n = unwords (drop n splitWords)

