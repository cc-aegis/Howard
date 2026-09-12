module Main where

import Lexer (derange, tokenize)

main :: IO ()
main = do
    let src = "main = print"
    let tokens = tokenize $ zip [0..] src
    print tokens