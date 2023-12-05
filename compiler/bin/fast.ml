(* Explicit-Free Abstract Syntax Tree *)
type name = Ast.name

type typename = name

type filename = Ast.filename

type ty = Uast.ty

type literal = Ast.literal

type pattern =
    Pattern of name * (name * ty) list
  | WildcardPattern

and expr =
    Literal of literal
  | Local of name
  | Global of name
  (* Case no longer implicity frees the top level of its scrutinee *)
  | Case of expr * (pattern * expr) list
  | If of expr * expr * expr
  | Let of name * expr * expr
  | Apply of expr * expr list
  | Dup of ty * name
  (* Memory-Related *)
  | Free of ty * name * expr (* Corresponds to a call to `free()` *)
  | FreeRec of ty * name * expr (* Corresponds to a call to "_free_" ^ (name_of ty) *)
  | Err of ty * string

type def =
    Define of name * ty * name list * expr
  | Datatype of name * (name * ty list) list

type program = def list
