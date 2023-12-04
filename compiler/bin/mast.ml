(* Explicitly Memory Managed Abstract Syntax Tree *)
type name = Ast.name

type filename = Ast.filename

(* ty is now LLVM types *)
(* Note: FunTy and Ptr might be redundant, we will see *)

type ty = Fun of ty * ty list | Int of int | Ptr of ty | Struct of ty list

type literal = Ast.literal

type pattern =
    Pattern of name * (name * ty) list
  | WildcardPattern (* hmmm perhaps names are not consumed if matched by wildcard? *)

and bind = name * expr

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of expr * (pattern * expr) list
  | If of expr * expr * expr
  | Let of name * expr * expr
  | Apply of expr * expr list
  (* Memory-Related *)
  | Free of name * expr
  (* Allocates a struct with a given tag and fields *)
  (* populated by the values bound to the names in the list *)
  | Alloc of ty * int * expr list
  | Err of string

type def = Define of name * ty * name list * expr
  (* Datatype definitions can be erased *)
  (* All necessary type information is encoded in the _alloc and _free functions *)

type program = def list

(* Pretty-printing functions *)

let rec string_of_ty = function 
   Fun(ty, tylist) ->
      "(-> (" ^ String.concat " " (List.map string_of_ty tylist) ^ ") " ^ string_of_ty ty ^ ")"
 | Int(int) -> "i" ^ string_of_int int 
 | Ptr(ty) -> string_of_ty ty ^ " *"
 | Struct(tylist) -> 
      "(struct (members " ^ String.concat " " (List.map string_of_ty tylist) ^ "))"

let string_of_lit lit = Ast.string_of_lit lit 

let is_int = String.for_all (function '0' .. '1' -> true | _ -> false)

let string_of_nameorwildcard (name, ty) =
  if is_int name then "_"
  else name

let string_of_pattern = function
   Pattern(name, nameorwildcardlist) -> 
     "(" ^ name ^ " " ^ String.concat " " (List.map string_of_nameorwildcard nameorwildcardlist) ^ ")"
 | WildcardPattern -> "_"

let rec string_of_expr = function 
   Literal(lit) -> string_of_lit lit 
 | Local(name) -> "%" ^ name 
 | Global(name) -> "@" ^ name 
 | Case(expr, casebranchlist) ->
     "(case " ^ string_of_expr expr ^ " (" ^ String.concat " " (List.map string_of_casebranch casebranchlist) ^ "))"
 | If(expr1, expr2, expr3) -> 
     "(if " ^ string_of_expr expr1 ^ " " ^ string_of_expr expr2 ^ " " ^ string_of_expr expr3 ^ ")"
 | Let(name, expr1, expr2) ->
     "(let " ^ "([%" ^ name ^ " " ^ string_of_expr expr1 ^ "]) " ^ string_of_expr expr2 ^ ")"
 | Apply(expr, exprlist) -> 
     "(" ^ string_of_expr expr ^ " " ^ String.concat " " (List.map string_of_expr exprlist) ^ ")"
 | Free(name, expr) -> 
     "(free %" ^ name ^ " " ^ string_of_expr expr ^ ")"
 | Alloc(ty, tag, exprlist) -> 
     "(alloc (type " ^ string_of_ty ty ^ ") " ^ string_of_int tag ^ " [ " ^ String.concat "; " (List.map string_of_expr exprlist) ^ " ] " 
 | Err(name) -> 
    "(err " ^ name ^ ")"
  
and string_of_bind = function 
   (name, expr) -> "[" ^ name ^ " " ^ string_of_expr expr ^ "]"
  
and string_of_casebranch = function 
   (pattern, expr) -> "[" ^ string_of_pattern pattern ^ " " ^ string_of_expr expr ^ "]"
  
let string_of_def = function 
 | Define(name, ty, namelist, expr) -> 
     "(define " ^ name ^ " (type " ^ string_of_ty ty ^ ") (" ^ String.concat " " namelist ^ ") " ^ string_of_expr expr ^ ")"

let string_of_program deflist = String.concat "\n" (List.map string_of_def deflist)
