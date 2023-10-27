(* Type-Checked Abstract Syntax Tree *)

type name = string

type filename = name

type ty = FunTy of (ty list) * ty | Int | Bool | Unit | Sym | CustomTy of name

type 'a typed = 'a * ty

type literal =
    IntLit of int
  | BoolLit of bool
  | SymLit of string
  | UnitLit

type nameorwildcard =
    PatternBindVar of name
  | WildcardBind

type pattern =
    Pattern of name * (nameorwildcard typed) list
  | WildcardPattern

type casebranch = CaseBranch of pattern * expr typed

and bind = name * expr typed

and expr =
    Literal of literal
  | NameExpr of name
  | Case of casebranch list
  | If of expr typed * expr typed * expr typed
  | Begin of (expr typed) list
  | Let of bind list * expr typed
  | Apply of (expr typed) * (expr typed) list
  | Dup of name

type variant = Variant of name * (ty list)

type def =
    Define of name * name list * expr typed
  | Datatype of name * (variant list)

type program = def list
