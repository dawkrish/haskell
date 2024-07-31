import Data.Char {- isSpace, isDigit, isAlphaNum -}
import Models

f = "{\n\t\"name\" : \"krish agarwal\",\n\t\"arr\" : [10,20,30]\n}"

m = trim f

wrapper :: String -> JSON
wrapper file = parse (trim file) 0

-- its half-validated.
parse :: String -> Int -> JSON
parse (s : ss) idx
  | isLeftCurly s = parseObject ss (idx + 1)

parseObject :: String -> Int -> JSON
parseObject ss idx
  | (ss !! idx) == '\"' = [Pair (k, v)]
  where
    (k, i1) = parseKey ss "" (idx + 1)
    (v, i2) = parseVal ss "" (i1 + 1)

parseKey :: String -> String -> Int -> (Key, Int)
parseKey ss acc idx
  | idx+1 > length ss- 1 = error "expected ':'"
  | currentLetter == '\"' && not (isColon (ss !! (idx+1))) = error "expected ':'"
  | currentLetter == '\"' = (Key acc, idx + 1)
  | otherwise = parseKey ss (acc ++ [currentLetter]) (idx + 1)
  where
    currentLetter = ss !! idx

parseVal :: String -> String -> Int -> (Val, Int)
parseVal ss acc idx
  | isQuote (ss!!idx) = parseLine ss "" (idx+1)

parseLine :: String -> String -> Int -> (Val, Int)
parseLine ss acc idx
  | currentLetter == '\"' = (Line acc, idx+1)
  | otherwise = parseLine ss (acc++[currentLetter]) (idx+1)
    where currentLetter = ss!!idx

valid :: String -> Bool
valid file = validateBrackets file "" "" && validateQuote file && validateIllegal file 0

validateIllegal :: String -> Int -> Bool
validateIllegal [] _ = True
validateIllegal (s : ss) q
  | s == '\"' = validateIllegal ss (q + 1)
  | odd q || (even q && legal s) = validateIllegal ss q
  | otherwise = error ("unknown literal '" ++ [s] ++ "'")
  where
    legal c = or [isSpace c, isAlphaNum c, isLeftCurly c, isRightCurly c, isLeftSquare c, isRightSquare c, isComma c, c == '-', c == ':']

validateBrackets :: String -> String -> String -> Bool
validateBrackets [] curlstk sqrstk
  | not (null curlstk) = error "expected '}'"
  | not (null sqrstk) = error "expected ']'"
  | otherwise = True
validateBrackets (s : ss) curlstk sqrstk
  | isLeftCurly s = validateBrackets ss (curlstk ++ [s]) sqrstk
  | isRightCurly s && null curlstk = error "Unexpected token '}'"
  | isRightCurly s && not (null curlstk) = validateBrackets ss (init curlstk) sqrstk
  | isLeftSquare s = validateBrackets ss curlstk (sqrstk ++ [s])
  | isRightSquare s && null sqrstk = error "Unexpected token ']'"
  | isRightSquare s && not (null sqrstk) = validateBrackets ss curlstk (init sqrstk)
  | otherwise = validateBrackets ss curlstk sqrstk

validateQuote :: String -> Bool
validateQuote file
  | (odd . length . filter (== '\"')) file = error ("quotes mismatch-> " ++ file)
  | otherwise = True

pairs :: String -> String -> String -> Int -> [String]
pairs (s:ss)

trim :: String -> String
trim file = [ch | ch <- spacesRemoved, ch /= '\n']
  where
    spacesRemoved = remSpc file 0

remSpc :: String -> Int -> String
remSpc [] _ = []
remSpc (s : ss) q
  | s == '\"' = s : remSpc ss (q + 1)
  | not (isSpace s) || (isSpace s && odd q) = s : remSpc ss q
  | otherwise = remSpc ss q

isLeftCurly :: Char -> Bool
isLeftCurly c = c == '{'

isRightCurly :: Char -> Bool
isRightCurly c = c == '}'

isLeftSquare :: Char -> Bool
isLeftSquare c = c == '['

isRightSquare :: Char -> Bool
isRightSquare c = c == ']'

isQuote :: Char -> Bool
isQuote c = c == '\"'

isComma :: Char -> Bool
isComma c = c == ','

isColon :: Char -> Bool
isColon c = c == ':'
