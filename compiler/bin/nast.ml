(* Normalized Abstract Syntax Tree *)

(* Author: Jasper Geer *)

type name = Ast.name

type filename = Ast.filename

type ty = Uast.ty

type literal = Ast.literal

type pattern =
    Pattern of name * (name * ty) list
  | Name of name

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of ty * name * (pattern * expr) list
  | If of name * expr * expr
  | Let of name * ty * expr * expr
  | Apply of name * name list
  | Dup of ty * name
  | Err of ty * string

type def =
    Define of name * ty * name list * expr
  | Datatype of name * (name * ty list) list

type program = def list
