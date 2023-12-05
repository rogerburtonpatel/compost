(* Association list of primitive function names and their types *)
let primitives =
  [
    (* I/O *)
    ("print-newline", Ast.FunTy ([], Ast.Unit));
    ("print-sym", Ast.FunTy ([Ast.Sym], Ast.Unit));
    ("print-int", Ast.FunTy ([Ast.Int], Ast.Unit));
    ("print-bool", Ast.FunTy ([Ast.Bool], Ast.Unit));
    ("print-unit", Ast.FunTy ([Ast.Unit], Ast.Unit));
    ("in", Ast.FunTy ([], Ast.Int));

    (* Equality *)
    ("=i", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Bool));
    ("=s", Ast.FunTy ([Ast.Sym; Ast.Sym], Ast.Bool));
    ("=b", Ast.FunTy ([Ast.Bool; Ast.Bool], Ast.Bool));
    ("=u", Ast.FunTy ([Ast.Unit; Ast.Unit], Ast.Bool));

    (* Arithmetic *)
    ("+", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("-", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("*", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("/", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("%", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Int));
    ("neg", Ast.FunTy ([Ast.Int], Ast.Int));

    (* Comparison *)
    (">", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Bool));
    ("<", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Bool));
    (">=", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Bool));
    ("<=", Ast.FunTy ([Ast.Int; Ast.Int], Ast.Bool));

    (* Boolean *)
    ("not", Ast.FunTy ([Ast.Bool], Ast.Bool));
    ("and", Ast.FunTy ([Ast.Bool; Ast.Bool], Ast.Bool));
    ("or", Ast.FunTy ([Ast.Bool; Ast.Bool], Ast.Bool));
    ("xor", Ast.FunTy ([Ast.Bool; Ast.Bool], Ast.Bool));
  ]
