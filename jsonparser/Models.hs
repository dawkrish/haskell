module Models where

data Val
  = Line String
  | Number String
  | Keyword String
  | Array [Val]
  | Obj Val
  deriving (Show)

newtype Key = Key String
  deriving (Show)

newtype Pair = Pair (Key, Val)
  deriving(Show)

type JSON = [Pair]
