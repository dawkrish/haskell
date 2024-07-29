import Data.Char {- isSpace, isDigit, isAlphaNum -}
import Models

f = "{\n\t\"name\" : \"krish agarwal\",\n\t\"arr\" : [10,20,30]\n}"

m = trim f

-- (valid . trim) file
parse :: String -> Maybe JSON
parse file = Nothing
  where
    ps = []

valid :: String -> Bool
valid file = validateBrackets file && validateIllegal file 0

validateIllegal :: String -> Int -> Bool
validateIllegal [] _ = True
validateIllegal (s : ss) q
  | s == '\"' = validateIllegal ss (q + 1)
  | odd q || (even q && legal s) = validateIllegal ss q
  | otherwise = error "unknown literal"
  where
    legal c = or [isSpace c, isAlphaNum c, isLeftCurly c, isRightCurly c, isLeftSquare c, isRightSquare c, isComma c, c == '-', c == ':']

validateBrackets :: String -> Bool
validateBrackets file
  | not (validateCurly file "") = error "curly brackets mismatch"
  | not (validateSquare file "") = error "square brackets mismatch"
  | not (validateQuote file) = error "quotes mismatch"
  | otherwise = True

validateQuote :: String -> Bool
validateQuote = even . length . filter (== '\"')

validateCurly :: String -> String -> Bool
validateCurly [] stk = null stk
validateCurly (s : ss) stk
  | isLeftCurly s = validateCurly ss (stk ++ [s])
  | isRightCurly s && not (null stk) = validateCurly ss (init stk)
  | isRightCurly s && null stk = False
  | otherwise = validateCurly ss stk

validateSquare :: String -> String -> Bool
validateSquare [] stk = null stk
validateSquare (s : ss) stk
  | isLeftSquare s = validateSquare ss (stk ++ [s])
  | isRightSquare s && not (null stk) = validateSquare ss (init stk)
  | isRightSquare s && null stk = False
  | otherwise = validateSquare ss stk

pairs :: String -> Int -> Int -> String -> [String]
pairs [] _ _ _ = []
pairs (s : ss) q a p
  | s == '\"' = pairs ss (q + 1) a (s : p)
  | even q && (s == '[' || s == ']') = pairs ss q (a + 1) (s : p)
  | odd q && (s == '[' || s == ']') = pairs ss q a (s : p)
  | s == ',' && (odd q || odd a) = pairs ss q a (s : p)

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

isComma :: Char -> Bool
isComma c = c == ','
