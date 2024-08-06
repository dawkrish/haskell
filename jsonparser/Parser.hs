import Data.Char {- isSpace, isDigit, isAlphaNum -}
import Models
import System.IO

main :: IO ()
main = do
  file <- readFile "test.json"
  print (wrapper file)

t1 = "{\n\t\"name\" : \"krish agarwal\",\n\t\"arr\" : [10,20,30]\n}"

t2 = "{\"name\" : \"dfd}s\"}"

t3 = "{\"perc\":-583.39e-1}"

t4 = "{\"name\" : \"alice\", \"age\" : 192}"

t5 = "{\"name\" : \"alice\", \"age\" : 192, \"student\" : true}"

t = trim t5

s = "\"abc\":"

wrapper :: String -> JSON
wrapper file
  | valid trimmed = parse trimmed 0
  | otherwise = []
  where
    trimmed = trim file

-- its half-validated.
parse :: String -> Int -> JSON
parse ss idx
  | isLeftCurly (ss !! idx) = fst (parseObject ss [] (idx + 1))

parseObject :: String -> JSON -> Int -> (JSON, Int)
parseObject ss js idx
  | isRightCurly (ss !! idx) = (js, idx)
  | isQuote (ss !! idx) = parseObject ss (js ++ [j]) i2
  | isComma (ss !! idx) && isRightCurly (ss !! (idx + 1)) = error "No comma(,) needed for one key-value pair"
  | isComma (ss !! idx) = parseObject ss js (idx + 1)
  | otherwise = error (show js ++ show idx)
  where
    (k, i1) = parseKey ss "" idx
    (v, i2) = parseVal ss "" i1
    j = Pair (k, v)

parseKey :: String -> String -> Int -> (Key, Int)
parseKey ss acc idx
  | (idx + 1) > length ss - 1 = error ("Index must go out of sight:/\t" ++ show idx ++ "\t" ++ show (idx + 1))
  | isQuote currentLetter && notElem '\"' acc = parseKey ss (acc ++ [currentLetter]) (idx + 1)
  | isQuote currentLetter && not (isColon (ss !! (idx + 1))) = error "key must terminate with colon(:)"
  | isQuote currentLetter = (Key (tail acc), idx + 2)
  | otherwise = parseKey ss (acc ++ [currentLetter]) (idx + 1)
  where
    currentLetter = ss !! idx

parseVal :: String -> String -> Int -> (Val, Int)
parseVal ss acc idx
  | isQuote currentLetter = parseLine ss "" idx
  | isMinus currentLetter = parseNumber ss "" idx
  | isDigit currentLetter = parseNumber ss "" idx
  | isAlpha currentLetter = parseKeyword ss "" idx
  | isLeftSquare currentLetter = parseArray ss [] idx
  | otherwise = error "Expecting a value"
  where
    currentLetter = ss !! idx

parseArray :: String -> [Val] -> Int -> (Val, Int)
parseArray ss acc idx
  | isRightSquare currentLetter = (Array acc, idx + 1)
  | isComma currentLetter = parseArray ss acc (idx+1)
  | otherwise = parseArray ss (acc ++ [val]) i1
  where
    (val, i1) = parseVal ss "" idx 
    currentLetter = ss !! idx

parseKeyword :: String -> String -> Int -> (Val, Int)
parseKeyword ss acc idx
  | (isRightCurly currentLetter || isComma currentLetter) && isValid acc = (Keyword acc, idx)
  | isRightCurly currentLetter || isComma currentLetter = error ("Unknown keyword " ++ acc)
  | isAlpha currentLetter = parseKeyword ss (acc ++ [currentLetter]) (idx + 1)
  | otherwise = error ("Unknown literal " ++ show currentLetter)
  where
    isValid s = s == "true" || s == "false" || s == "null"
    currentLetter = ss !! idx

{-
  Takes starting index to be '"' and keep accumulating till it finds another '"'
  Returns (tail acc, newIdx)
  where newIdx is idx after the last '"'
-}
parseLine :: String -> String -> Int -> (Val, Int)
parseLine ss acc idx
  | isQuote currentLetter && notElem '\"' acc = parseLine ss (acc ++ [currentLetter]) (idx + 1)
  | isQuote currentLetter = (Line (tail acc), idx + 1)
  | otherwise = parseLine ss (acc ++ [currentLetter]) (idx + 1)
  where
    currentLetter = ss !! idx

parseNumber :: String -> String -> Int -> (Val, Int)
parseNumber ss acc idx
  | idx >= length ss = error "index large :("
  | (isRightCurly currentLetter || isComma currentLetter) && isDigit (last acc) = (Number acc, idx)
  | (isRightCurly currentLetter || isComma currentLetter) && isExp (last acc) = error "Unterminated exponent(e)"
  | (isRightCurly currentLetter || isComma currentLetter) && isDot (last acc) = error "Unterminated float(.)"
  | (isRightCurly currentLetter || isComma currentLetter) && isMinus (last acc) = error "Unterminated negative(-) number"
  | isMinus currentLetter && (null acc || isExp (last acc)) = nextCall
  | isMinus currentLetter = error "Expected exponent(e) before negative(-)"
  | isDigit currentLetter = nextCall
  | isDot currentLetter && notElem '.' acc && notElem 'e' acc = nextCall
  | isDot currentLetter && elem '.' acc = error "can't have two floating points :("
  | isDot currentLetter && elem 'e' acc = error "float(.) must come before exponent(e)"
  | isExp currentLetter && notElem 'e' acc && isDigit (last acc) = nextCall
  | isExp currentLetter && elem 'e' acc = error "can't have two exponents :("
  | isExp currentLetter && not (isDigit (last acc)) = error "expected number before exponent(e)"
  | otherwise = error ("Invalid literal " ++ [currentLetter])
  where
    currentLetter = ss !! idx
    nextCall = parseNumber ss (acc ++ [currentLetter]) (idx + 1)

valid :: String -> Bool
valid file = validCurly file "" 0 && validSquare file "" 0 && validateQuote file && validateIllegal file 0

validCurly :: String -> String -> Int -> Bool
validCurly [] stk q
  | not (null stk) = error "Expected token '}'"
  | otherwise = True
validCurly (s : ss) stk q
  | isQuote s = validCurly ss stk (q + 1)
  | odd q = validCurly ss stk q
  | isLeftCurly s = validCurly ss (stk ++ [s]) q
  | isRightCurly s && null stk = error "Unexpected token '}'"
  | isRightCurly s && not (null stk) = validCurly ss (init stk) q
  | otherwise = validCurly ss stk q

validSquare :: String -> String -> Int -> Bool
validSquare [] stk q
  | not (null stk) = error "Expected token ']'"
  | otherwise = True
validSquare (s : ss) stk q
  | isQuote s = validSquare ss stk (q + 1)
  | odd q = validSquare ss stk q
  | isLeftSquare s = validSquare ss (stk ++ [s]) q
  | isRightSquare s && null stk = error "Unexpected token ']'"
  | isRightSquare s && not (null stk) = validSquare ss (init stk) q
  | otherwise = validSquare ss stk q

validateIllegal :: String -> Int -> Bool
validateIllegal [] _ = True
validateIllegal (s : ss) q
  | s == '\"' = validateIllegal ss (q + 1)
  | odd q || (even q && legal s) = validateIllegal ss q
  | otherwise = error ("unknown literal '" ++ [s] ++ "'")
  where
    legal c = or [isSpace c, isAlphaNum c, isLeftCurly c, isRightCurly c, isLeftSquare c, isRightSquare c, isComma c, isMinus c, isDot c, isColon c]

validateQuote :: String -> Bool
validateQuote file
  | (odd . length . filter (== '\"')) file = error ("quotes mismatch-> " ++ file)
  | otherwise = True

-- pairs :: String -> String -> String -> Int -> [String]

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

isMinus :: Char -> Bool
isMinus c = c == '-'

isDot :: Char -> Bool
isDot c = c == '.'

isExp :: Char -> Bool
isExp c = c == 'e'
