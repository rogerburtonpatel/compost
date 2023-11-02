(* Unambiguous Abstract Syntax Tree *)
type name = string

type filename = name (* NEEDSWORK: Decide if we want different rules for filenames *)

type ty = Ast.ty

type literal = Ast.literal

type pattern = Ast.pattern

type casebranch = CaseBranch of pattern * expr

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of expr * (casebranch list)
  | If of expr * expr * expr
  | Begin of expr * expr
  | Let of name * expr * expr
  | Apply of expr * (expr list)
  | Dup of name

type variant = Variant of name * (ty list)

type def =
    Val of name * expr
  | Define of name * (name list) * expr
  | Datatype of name * (variant list)
  | TyAnnotation of name * ty

type program = def list
