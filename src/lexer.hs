module Lexer where

import Token (Token(..))
import Error (CompilerError(..), CompilerResult(..), Result(..))

import Data.Char (isSpace)

tokenize :: Enumerate Char -> CompilerResult [Ranged Token]
tokenize src
    | nextToken == Err Eof = Ok []
    | otherwise = do
        (token, srcRest) <- next src
        tokenRest <- tokenize srcRest
        Ok (token : tokenRest)
    where nextToken = next src

data Range a = Range a a
    deriving (Show, Eq)
type Enumerate a = [(Int, a)]
type Ranged a = (Range Int, a)

derange :: Enumerate a -> Ranged [a]
derange [] = undefined
derange as = (Range start end, fmap (\(_, a) -> a) as)
    where
        (start, _) = head as
        (end, _) = last as

isLowerCase :: Char -> Bool
isLowerCase c = 'a' <= c && c <= 'z'

isUpperCase :: Char -> Bool
isUpperCase c = 'A' <= c && c <= 'Z'

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

headSatisfies :: (a -> Bool) -> [a] -> Bool
headSatisfies _ [] = False
headSatisfies cond (a:_) = cond a

next :: Enumerate Char -> CompilerResult (Ranged Token, Enumerate Char)
next [] = Err Eof
next ((idx, c):cs)
    | c == '\n' && headSatisfies (\(_, c) -> not $ isSpace c) cs = Ok ((Range idx idx, DefSep), cs)
    | isSpace c = next cs
    | isLowerCase c = parseIdent ((idx, c):cs)
    -- | isUpperCase c = parseTypeIdent ((idx, c):cs)
    -- | isDigit c = parseNumber ((idx, c):cs)
    -- | c == '"' = parseString ((idx, c):cs)
    | otherwise = parseOperator ((idx, c):cs)

parseIdent :: Enumerate Char -> CompilerResult (Ranged Token, Enumerate Char)
parseIdent src = Ok ((range, Ident ident), rest)
    where
        (indexedIdent, rest) = span (\(_, c) -> isLowerCase c || isUpperCase c) src
        (range, ident) = derange indexedIdent

parseOperator :: [(Int, Char)] -> CompilerResult (Ranged Token, [(Int, Char)])
-- parseOperator ((_, ':'):(_, ':'):rest) = Ok (Signature, rest)
parseOperator ((idx, '='):rest) = Ok ((Range idx idx, Assign), rest)
parseOperator ((idx, c):rest) = Err (UnexpectedChar idx)
