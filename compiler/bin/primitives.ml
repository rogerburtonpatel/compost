(* Association list of primitive function names and their types *)
let primitives =
  [
    (* I/O *)
    ("print-newline", Uast.FunTy ([], Uast.Unit));
    ("print-sym", Uast.FunTy ([Uast.Sym], Uast.Unit));
    ("print-int", Uast.FunTy ([Uast.Int], Uast.Unit));
    ("print-ascii", Uast.FunTy ([Uast.Int], Uast.Unit));
    ("print-bool", Uast.FunTy ([Uast.Bool], Uast.Unit));
    ("print-unit", Uast.FunTy ([Uast.Unit], Uast.Unit));
    ("in", Uast.FunTy ([], Uast.Int));

    (* Equality *)
    ("=i", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Bool));
    ("=s", Uast.FunTy ([Uast.Sym; Uast.Sym], Uast.Bool));
    ("=b", Uast.FunTy ([Uast.Bool; Uast.Bool], Uast.Bool));
    ("=u", Uast.FunTy ([Uast.Unit; Uast.Unit], Uast.Bool));

    (* Arithmetic *)
    ("+", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Int));
    ("-", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Int));
    ("*", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Int));
    ("/", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Int));
    ("%", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Int));
    ("neg", Uast.FunTy ([Uast.Int], Uast.Int));

    (* Comparison *)
    (">", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Bool));
    ("<", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Bool));
    (">=", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Bool));
    ("<=", Uast.FunTy ([Uast.Int; Uast.Int], Uast.Bool));

    (* Boolean *)
    ("not", Uast.FunTy ([Uast.Bool], Uast.Bool));
    ("and", Uast.FunTy ([Uast.Bool; Uast.Bool], Uast.Bool));
    ("or", Uast.FunTy ([Uast.Bool; Uast.Bool], Uast.Bool));
    ("xor", Uast.FunTy ([Uast.Bool; Uast.Bool], Uast.Bool));
  ]

let primitive_tys =
  [
    ("int", Uast.Int);
    ("bool", Uast.Bool);
    ("unit", Uast.Unit);
    ("sym", Uast.Sym);
  ]
