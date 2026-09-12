module Token where

-- minimal token def for hello world
data Token =
    Ident String
    | StringLit String
    | Assign

-- main = print "Hello World"
-- [Ident "main", Assign, Ident "print", StringLit "Hello World"]