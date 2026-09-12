import Ast

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = case f x of
    Just y -> Just y
    Nothing -> firstJust f xs

eval :: [Definition] -> AstItem -> IO AstItem
eval _ (AstConst atom) = IO (AstConst atom)
eval _ (AstInvocation (Function "add") [AstConst x, AstConst y]) = IO (AstConst x + y)
eval _ (AstInvocation (Function "mul") [AstConst x, AstConst y]) = IO (AstConst x * y)
eval _ (AstInvocation (Function "sub") [AstConst x, AstConst y]) = IO (AstConst x - y)
eval _ (AstInvocation (Function "div") [AstConst x, AstConst y]) = IO (AstConst x / y)
eval _ (AstInvocation (Function "print") [AstConst num]) = print num
-- TODO: eval all children -> pattern lookup -> eval result
eval defs (AstInvocation function parameters) = do
    -- eval parameters
    parameters' <- traverse (eval defs) parameters
    -- pattern lookup

    -- call eval on resulting pattern

tryMatch :: Definition -> (Ident, [AstItem]) -> Maybe AstItem
tryMatch (Definition defFunc defParams defResult) (func, params)
    | defFunc /= func = Nothing
    | length params < length defParams = Nothing
    | otherwise = do
        bindings <- fmap concat $ traverse (uncurry matchSingle) (zip defParams params)
    | -- params will be astitems AND bindings which will have to be placed into result

matchSingle :: PatItem -> AstItem -> Maybe [(Ident, AstItem)]
matchSingle (AstConst lhs) (AstConst rhs)
    | lhs == rhs = Just []
    | otherwise = Nothing
matchSingle (PatBinding name) rhs = Just [(name, rhs)]

replace :: [(Ident, AstItem)] -> AstItem -> AstItem
replace bindings 


-- IGNORE
--eval defs (Invocation f args) = do
--    evaluatedArgs <- traverse (eval defs) args
--    eval defs (Invocation f args)