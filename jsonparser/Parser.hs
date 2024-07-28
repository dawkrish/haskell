import Models

f = "{\n\"name\" : \"krish agarwal\"\n}"

parse :: String -> Maybe JSON
parse file = Nothing

trim :: String -> String
trim file = [ch | ch <- spacesRemoved, ch /= '\n']
  where
    spacesRemoved = file
