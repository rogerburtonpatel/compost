(* Type-Checked Abstract Syntax Tree *)
(* open Ast *)

type name = Ast.name

type filename = Ast.filename

type ty = Ast.ty

type literal = Ast.literal

type 'a typed = 'a * ty

type pattern = Ast.pattern

type casebranch = CaseBranch of pattern * expr typed

and bind = name * expr typed

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of expr typed * casebranch list
  | If of expr typed * expr typed * expr typed
  | Begin of expr typed * expr typed
  | Let of name * expr typed * expr typed
  | Apply of expr typed * (expr typed) list
  | Dup of name

type variant = Variant of name * (ty list)

type def =
    Define of name * (name typed) list * expr typed
  | Datatype of name * (variant list)

type program = def list
