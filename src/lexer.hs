module Lexer where

import Token (Token)
import Error (CompilerError, CompilerResult)

import Data.Char (isSpace)
import Data.Range (Range)

-- tokenize :: String -> CompilerResult [Ranged Token]

type Enumerate a = [(Int, a)]
type Ranged a = (Range Int, a)

derange :: Enumerate a => Ranged [a]
derange [] = undefined
derange as = (SpanRange start end, fmap (\(_, a) -> a) as)
    where
        (start, _) = first as
        (end, _) = last as

isLowerCase :: Char -> Bool
isLowerCase c = 'a' <= c && c <= 'z'

isUpperCase :: Char -> Bool
isUpperCase c = 'A' <= c && c <= 'Z'

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

next :: Enumerate Char -> CompilerResult (Token, Enumerate Char)
next "" = Err Eof
next ((idx, c):cs)
    | isSpace c = next cs
    | isLowerCase c = parseIdent ((idx, c):cs)
    -- | isUpperCase c = parseTypeIdent ((idx, c):cs)
    -- | isDigit c = parseNumber ((idx, c):cs)
    -- | c == '"' = parseString ((idx, c):cs)
    | _ = parseOperator ((idx, c):cs)

parseIdent :: Enumerate Char -> CompilerResult (Token, Enumerate Char)
parseIdent src = case fmap (\(_, c) -> c) ident of
    | "" -> undefined -- TODO
    | _ -> undefined -- TODO
    where (ident, rest) = span (\c -> isLowerCase c || isUpperCase c) src

parseOperator :: [(Int, Char)] -> CompilerResult (Token, [(Int, Char)])
-- parseOperator ((_, ':'):(_, ':'):rest) = Ok (Signature, rest)
parseOperator ((idx, '='):rest) = Ok ((SpanRange idx idx, Assign), rest)
parseOperator ((idx, c):rest) = Err UnexpectedChar idx
