module Utils where

data Range a = Range a a
    deriving (Show, Eq)
type Enumerate a = [(Int, a)]
type Ranged a = (Range Int, a)