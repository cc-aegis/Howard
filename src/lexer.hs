import Data.Char (isSpace)

type Enumerate a = [(Int, a)]

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
    | isUpperCase c = parseTypeIdent ((idx, c):cs)
    | isDigit c = parseNumber ((idx, c):cs)
    -- | c == '"' = parseString ((idx, c):cs)
    | _ = parseOperator ((idx, c):cs)

parseIdent :: Enumerate Char -> CompilerResult (Token, Enumerate Char)
parseIdent src = case fmap (\(_, c) -> c) ident of
    | ""
    where (ident, rest) = span (\c -> isLowerCase c || isUpperCase c) src

parseOperator :: [(Int, Char)] -> CompilerResult (Token, [(Int, Char)])
parseOperator ((_, ':'):(_, ':'):rest) = Ok (Signature, rest)
parseOperator ((_, '='):rest) = Ok (Assign, rest)
parseOperator ((idx, c):rest) = Err UnexpectedChar idx
