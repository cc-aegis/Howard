import Error (CompilerError, CompilerResult, Error)
import Utils (Ranged)

next :: [Ranged Token] -> CompilerResult (Ranged AstItem, [Ranged Token])
next [] = Err Eof