combo :: Eq a => [a] -> [[a]]
combo xs = foldl (\acc idx -> acc ++ map (++[xs!!idx]) (filter (notElem (xs!!(idx-1))) acc) ) [[]] [0..length xs - 1]

split :: String -> Char -> [String]
split line d = foldl (\acc x -> if x /= d then init acc ++ [last acc ++ [x]] else acc ++ [[]]) [[]] line
