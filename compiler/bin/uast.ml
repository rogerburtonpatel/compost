(* Unambiguous Abstract Syntax Tree *)

type name = string

type filename = name (* NEEDSWORK: Decide if we want different rules for filenames *)

type ty = FunTy of (ty list) * ty | Int | Bool | Unit | Sym | CustomTy of name

type literal = Ast.literal

type nameorwildcard =
    PatternBindVar of name
  | WildcardBind

type pattern =
    Pattern of name * nameorwildcard list
  | WildcardPattern

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
  | Use of filename

type program = def list
