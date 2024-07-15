perm :: String -> [String]
perm s
    | length s == 1 = [s]
    | length s == 2 = [s,reverse s]
    | otherwise = concat (map f zs)
     where
      xs = map (\x -> [x]) s
      ys = dropOnce s
      zs = zip xs ys
      f = (\(a,b) -> map (\x -> a++x) (perm b))

dropOnce :: String -> [String]
dropOnce xs = [take i xs ++ drop (i+1) xs | i <- [0..length xs - 1]]
