module Error where

type CompilerResult = Result CompilerError

data CompilerError = Eof | UnexpectedChar Int
    deriving (Show, Eq)

data Result err val = Err err | Ok val 
    deriving (Show, Eq)

instance Functor (Result err) where
    fmap _ (Err err) = Err err
    fmap f (Ok val) = Ok (f val)

instance Applicative (Result err) where
    pure x = Ok x
    
    (Ok f) <*> (Ok val) = Ok (f val)
    (Err err) <*> _ = Err err
    _ <*> (Err err) = Err err

instance Monad (Result err) where
    (Ok val) >>= f = f val
    (Err err) >>= _ = (Err err)