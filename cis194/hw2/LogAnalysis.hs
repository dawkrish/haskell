module LogAnalysis where

import Log

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ms = map (\(LogMessage _ _ s) -> s) (inOrder (build ys))
 where ys =  filter errGreaterEqual50 ms

errGreaterEqual50 :: LogMessage -> Bool
errGreaterEqual50 (LogMessage (Error x) _ _) | x >= 50 = True
errGreaterEqual50 _ = False

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt m rt) = inOrder lt ++ [m] ++ inOrder rt

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown msg) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert m@(LogMessage _ newTime _) (Node lt t@(LogMessage _ rootTime _ ) rt)
  | newTime < rootTime = Node (insert m lt) t rt
  | otherwise = Node lt t (insert m rt)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build ms = insert (last ms) (build (init ms))

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

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

