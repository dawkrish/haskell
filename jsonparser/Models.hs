module Models where

data Val
  = Line String
  | Number Int
  | Constant
  | JSON
  | Array
  deriving (Show)

data Constant = TRUE | FALSE | NULL

instance Show Constant where
  show TRUE = "true"
  show FALSE = "false"
  show NULL = "null"

newtype Key = Key String

newtype Pair = Pair (Key, Val)

type JSON = [Pair]
