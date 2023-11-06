(* Association list of primitive function names and their types *)
let primitives =
  [
    ("print-sym", Ast.FunTy ([Ast.Sym], Ast.Unit));
    ("print-int", Ast.FunTy ([Ast.Int], Ast.Unit));

    (* Equality *)
    ("i=", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Bool));
    ("s=", Ast.FunTy ([Ast.Sym; Ast.Sym], Ast.Bool));
    ("b=", Ast.FunTy ([Ast.Bool; Ast.Bool], Ast.Bool));
    ("u=", Ast.FunTy ([Ast.Unit; Ast.Unit], Ast.Bool));

    (* Arithmetic *)
    ("+", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("-", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("*", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("/", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("%", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("neg", Ast.FunTy ([Ast.Int], Ast.Int));
  ]
