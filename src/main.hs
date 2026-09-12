module Main where

import Lexer (derange, tokenize)

main :: IO ()
main = do
    let src = "main = print 0315"
    print $ tokenize $ zip [0..] src