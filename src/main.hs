module Main where

import Lexer (derange)

main :: IO ()
main = do
    print $ span (\c -> c < 'e') "abcdef"
    print $ derange [(3, 't'), (4, 'e'), (5, 'r'), (6, 'e')]