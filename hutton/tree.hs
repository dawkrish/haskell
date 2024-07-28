data Tree = Leaf | Node Tree Int Tree
    deriving (Show, Eq)

--t = Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)) 10 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 (Node Leaf 7 Leaf)))
t = Node (Node Leaf 1 (Node Leaf 4 Leaf)) 10 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 (Node Leaf 7 Leaf)))

ss = call t
p = ss!!0
q = ss!!1
r = ss!!2
s = ss!!3

--final = (repr p 2) ++ (repr q 1) ++ (repr r 0)
final = (repr p 3) ++ (repr q 2) ++ (repr r 1) ++ (repr s 0)

drawTree :: Tree -> String
drawTree t = foldl (\acc (rep,lvl)-> acc ++ repr rep lvl) "" reprs 
    where reprs = zip ss [length ss - 1, length ss -2..]
          ss = modify (call t)

modify :: [[String]] -> [[String]]
modify sss = map (map f) sss 
    where m = maximum  [length s | ss <- sss, s <-ss]            
          f "*" = concat ["*" |_ <- [0..(m-1)]]
          f  x  = concat ["0" |_ <- [0..(m-length x -1)]] ++ x

          

repr :: [String] -> Int -> String
repr letters lvl
    |length letters == 1 = snd
    |otherwise = fst ++ snd 
        where fst = sidepad ++ init (connectors) ++ sidepad ++ "\n"
              snd = sidepad ++ vals ++ sidepad ++ "\n" 
              sidepad = concat [" " | i <- [0..(2^lvl - 2)]]
              middlepad = concat [" " | i <- [0..(2^(lvl+1) - 2)]]
              vals = concat [l ++ middlepad | l <- letters]
              connectors = concat [if i `mod` 2 == 0 then "/" ++ middlepad else "\\" ++ middlepad| i<-[0..length letters - 1]]



val :: Tree -> String
val Leaf = "*"
val (Node _ val _) = show val

call :: Tree -> [[String]]
call Leaf = []
call n@(Node _ _ _) = bfs [n]

bfs :: [Tree] -> [[String]]
bfs ts 
    | ts == take (length ts) (repeat Leaf) = []
    | otherwise = [level] ++  bfs newts
        where level = foldl (\acc x -> acc ++ [(val x)]) [] ts
              newts = foldl (\acc n-> f acc n) [] ts
              f acc Leaf = acc ++ [Leaf] ++ [Leaf]
              f acc (Node lt _ rt) = acc ++ [lt] ++ [rt]

--ts == take (length ts) (repeat Leaf) = [take (length ts) (repeat (val Leaf))]
