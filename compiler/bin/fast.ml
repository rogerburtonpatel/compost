(* Explicit-Free Abstract Syntax Tree *)
type name = Ast.name

type typename = name

type filename = Ast.filename

type ty = Ast.ty

type literal = Ast.literal

type pattern =
    Pattern of name * name list
  | WildcardPattern (* hmmm perhaps names are not consumed if matched by wildcard? *)

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of ty * expr * (pattern * expr) list
  | If of expr * expr * expr
  | Begin of expr * expr
  | Let of name * expr * expr
  | Apply of expr * expr list
  | Dup of name
  (* Memory-Related *)
  | FreeRec of ty * name * expr (* Corresponds to a call to "_free_" ^ (name_of ty) *)
  | Free of ty * name * expr (* Corresponds to a call to `free()` *)

type def =
    Define of name * ty * name list * expr
  | Datatype of name * (name * ty list) list

type program = def list
