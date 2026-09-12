data CompilerError = Eof | UnexpectedChar Int

data CompilerResult value error = Ok value | Err error