data Ident = Ident String
data Atomic = AtNumber Integer | AtIO

data AstItem = AstInvocation Ident [AstItem] | AstConst Atomic
data PatItem = PatBinding Ident | PatConst Atomic

data Definition = Definition Ident [PatItem] AstItem