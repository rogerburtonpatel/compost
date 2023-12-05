(* Type-Checked Abstract Syntax Tree *)
(* open Ast *)

type name = Ast.name

type filename = Ast.filename

type ty = Ast.ty

type literal = Ast.literal

type pattern =
    Pattern of name * (name * ty) list
  | Name of name * bool

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of ty * expr * (pattern * expr) list
  | If of expr * expr * expr
  | Let of name * ty * expr * expr
  | Apply of expr * expr list
  | Dup of ty * name
  | Err of ty * string

type def =
    Define of name * ty * name list * expr
  | Datatype of name * (name * ty list) list

type program = def list
