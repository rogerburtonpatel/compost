(* Explicitly Memory Managed Abstract Syntax Tree *)
type name = Ast.name

type filename = Ast.filename

(* ty is now LLVM types *)
(* Note: FunTy and Ptr might be redundant, we will see *)

type ty = Fun of ty * ty list | Int of int | Ptr of ty | Struct of ty list

type literal = Ast.literal

type pattern =
    Pattern of name * name list
  | WildcardPattern (* hmmm perhaps names are not consumed if matched by wildcard? *)

and bind = name * expr

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of ty * expr * (pattern * expr) list
  | If of expr * expr * expr
  | Begin of expr * expr
  | Let of name * expr * expr
  | Apply of expr * expr list
  (* Memory-Related *)
  | Free of name * expr
  (* Allocates a struct with a given tag and fields *)
  (* populated by the values bound to the names in the list *)
  | Alloc of ty * int * expr list

type def = Define of name * ty * name list * expr
  (* Datatype definitions can be erased *)
  (* All necessary type information is encoded in the _alloc and _free functions *)

type program = def list
