let primitives =
  [
    ("print-sym", Ast.FunTy ([Ast.Sym], Ast.Unit));
    ("print-int", Ast.FunTy ([Ast.Int], Ast.Unit))
  ]
