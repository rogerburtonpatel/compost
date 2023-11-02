(* Explicitly Memory Managed Abstract Syntax Tree *)
open Ast

type name = Ast.name

type filename = Ast.filename

(* ty is now LLVM types *)
(* Note: FunTy and Ptr might be redundant, we will see *)

type ty = Fun of ty * ty list | Int of int | Ptr of ty | Struct of ty list

type 'a typed = 'a * ty

type literal = Ast.literal

type pattern =
    Pattern of name * (name typed) list
  | WildcardPattern (* hmmm perhaps names are not consumed if matched by wildcard? *)

type casebranch = CaseBranch of pattern * expr typed

and bind = name * expr typed

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of name typed * casebranch list
  | If of expr typed * expr typed * expr typed
  | Begin of expr typed * expr typed
  | Let of name * expr typed * expr typed
  | Apply of expr typed * (expr typed) list
  (* Memory-Related *)
  | Free of ty * name * expr typed
  (* Allocates a struct with a given tag and fields *)
  (* populated by the values bound to the names in the list *)
  | Alloc of int * (expr typed) list

type def = Define of name * (name typed) list * expr typed
  (* Datatype definitions can be erased *)
  (* All necessary type information is encoded in the _alloc and _free functions *)

type program = def list
