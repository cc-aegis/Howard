import Error (CompilerError, CompilerResult, Result(..))
import Token (Token(..))
import Utils (Ranged)

parseProgram :: [Ranged Token] -> CompilerResult [Ranged Definition]
parseProgram [] = Ok []
parseProgram ((_, DefSep):rest) = parseProgram rest
parseProgram tokens = do
    (first, tokens') <- parseDefinition tokens
    rest <- parseProgram tokens'
    Ok (first:rest)

parseDefinition :: [Ranged Token] -> CompilerResult (Ranged Definition, [Ranged Token])
parseDefinition tokens = do
    (name, tokens') <- parseIdent tokens
    (patterns, tokens'') <- parseWhile (!= Assign) parsePattern tokens
    tokens''' <- expectToken (== Assign) tokens''
    (body, tokens'''') <- parseExpr tokens'''
    TODO

parseExpr :: [Ranged Token] -> CompilerResult (Ranged AST)

parseParens :: [Ranged Token] -> CompilerResult (Ranged AST, [Ranged Token])
parseParens tokens = do
    tokens' <- expectToken (== LParen) tokens
    body <- parseExpr tokens'
    tokens'' <- expectToken (== LParen) tokens'
    Ok (body, tokens'')


parseWhile :: (Token -> Bool) -> ([Ranged Token] -> CompilerResult (a, [Ranged Token])) -> [Ranged Token] -> CompilerResult ([a], [Ranged Token])
parseWhile _ _ [] = Ok []
parseWhile cond parse ((range, token):tokens)
    | not $ cond token = Ok []
    | otherwise = do
        (first, tokens') <- parse ((range, token):tokens)
        (rest, tokens'') <- parseWhile cond parse tokens'
        Ok (first:rest, tokens'')

parsePattern :: [Ranged Token] -> CompilerResult (Ranged PatItem, [Ranged Token])
parsePattern [] = Err Eof
parsePattern ((range, Ident ident):tokens) = Ok ((range, PatBinding ident), tokens)
parsePattern ((range, Number number):tokens) = Ok ((range, PatConst (AtNumber number)), tokens)

expectToken :: (Token -> Bool) -> [Ranged Token] -> CompilerResult [Ranged Token]
expectToken _ [] = False
expectToken f ((range, t):ts) = if f t then Ok ts else Err (UnexpectedToken range t)

parseIdent :: [RangedToken] -> CompilerResult (Ranged String, [RangedToken])
parseIdent [] = Err Eof
parseIdent ((range, Ident ident):tokens) = Ok ((range, ident), tokens)
parseIdent ((range, token):_) = Err (UnexpectedToken range token)