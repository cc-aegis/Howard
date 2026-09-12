module Error where

data CompilerError = Eof | UnexpectedChar Int

data CompilerResult value error = Ok value | Err error

instance Monad CompilerResult where
    return = Ok
    (Ok x) >>= f = f x
    (Err err) >>= f = Err (err)