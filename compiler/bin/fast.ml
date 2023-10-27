(* Explicit-Free Abstract Syntax Tree *)
open Ast

type name = Ast.name

type filename = Ast.filename

type ty = Ast.ty

type literal = Ast.literal

type 'a typed = 'a * ty

type pattern =
    Pattern of name
  | WildcardPattern (* hmmm perhaps names are not consumed if matched by wildcard? *)

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
  (* Memory-Related *)
  | Free of ty * name * expr typed (* read this as free `name`s then do `expr` *)
  | ConsArg of name * int

type variant = Variant of name * (ty list)

type def =
    Define of name * name list * expr typed
  | Datatype of name * (variant list)

type program = def list
