(* Explicitly Memory Managed Abstract Syntax Tree *)
open Ast

type name = Ast.name

type filename = Ast.filename

(* ty is now LLVM types *)
(* Note: FunTy and Ptr might be redundant, we will see *)
type ty = FunTy | IntTy of int | Ptr | StructTy of ty list

type 'a typed = 'a * ty

type literal =
    IntLit of int
  | BoolLit of bool
  | SymLit of string
  | UnitLit

type pattern =
    Pattern of int
  | WildcardPattern (* hmmm perhaps names are not consumed if matched by wildcard? *)

type casebranch = CaseBranch of pattern * expr typed

and bind = name * expr typed

and expr =
    Literal of literal
  | NameExpr of name
  | Case of expr typed * casebranch list
  | If of expr typed * expr typed * expr typed
  | Begin of expr typed * expr typed
  | Let of name * expr typed * expr typed
  | Apply of expr typed * (expr typed) list
  (* Memory-Related *)
  | Free of ty * name * expr typed
  (* Allocates a struct with a given tag and fields *)
  (* populated by the values bound to the names in the list *)
  | Alloc of int * name list
  | ConsArg of name * int

type def = Define of name * name list * expr typed
  (* Datatype definitions can be erased *)
  (* All necessary type information is encoded in the _alloc and _free functions *)

type program = def list
