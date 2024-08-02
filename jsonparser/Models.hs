module Models where

data Val
  = Line String
  | Number String
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
  deriving (Show)

newtype Pair = Pair (Key, Val)
  deriving(Show)

type JSON = [Pair]
