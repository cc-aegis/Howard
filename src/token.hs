module Token where

-- minimal token def for hello world
data Token =
    Ident String
    | StringLit String
    | Assign
    deriving (Show, Eq)

-- main = print "Hello World"
-- [Ident "main", Assign, Ident "print", StringLit "Hello World"]