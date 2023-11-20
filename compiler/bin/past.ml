(* Processed Abstract Syntax Tree and functions for printing it *)

type name = Ast.name

type ty = Ast.ty

type literal = Ast.literal

type pattern = Ast.pattern

and expr =
    Literal of literal 
  | NameExpr of name 
  | Case of expr * (pattern * expr) list
  | If of expr * expr * expr 
  | Let of name * expr * expr
  | Apply of expr * (expr list) 
  | Dup of name 

type def =
    Val of name * expr
  | Define of name * (name list) * expr 
  | Datatype of name * (name * ty list) list
  | TyAnnotation of name * ty 

type program = def list


(* Pretty printing functions *)

let string_of_namelist = Ast.string_of_namelist
let string_of_ty = Ast.string_of_ty
let string_of_symlit = Ast.string_of_symlit
let string_of_lit = Ast.string_of_lit
let is_int = Ast.is_int
let string_of_nameorwildcard = Ast.string_of_nameorwildcard
let string_of_pattern = Ast.string_of_pattern
let string_of_variant = Ast.string_of_variant

let rec string_of_expr = function 
   Literal(lit) -> string_of_lit lit 
 | NameExpr(name) -> name 
 | If(expr1, expr2, expr3) -> 
     "(if " ^ string_of_expr expr1 ^ " " ^ string_of_expr expr2 ^ " " ^ string_of_expr expr3 ^ ")"
 | Let(name, expr1, expr2) ->
     "(let " ^ "([" ^ name ^ " " ^ string_of_expr expr1 ^ "]) " ^ string_of_expr expr2 ^ ")"
 | Apply(expr, exprlist) -> 
     "(" ^ string_of_expr expr ^ " " ^ String.concat " " (List.map string_of_expr exprlist) ^ ")"
 | Case(expr, casebranchlist) ->
     "(case " ^ string_of_expr expr ^ " (" ^ String.concat " " (List.map string_of_casebranch casebranchlist) ^ "))"
 | Dup(name) ->
     "(dup " ^ name ^ ")"

and string_of_bind = function 
   (name, expr) -> "[" ^ name ^ " " ^ string_of_expr expr ^ "]"
  
and string_of_casebranch = function 
   (pattern, expr) -> "[" ^ string_of_pattern pattern ^ " " ^ string_of_expr expr ^ "]"

let string_of_def = function 
   Val(name, expr) -> "(val " ^ name ^ " " ^ string_of_expr expr ^ ")"
 | Define(name, namelist, expr) -> 
     "(define " ^ name ^ " (" ^ String.concat " " namelist ^ ") " ^ string_of_expr expr ^ ")"
 | Datatype(name, variantlist) ->
     "(datatype " ^ name ^ " (" ^ String.concat " " (List.map string_of_variant variantlist) ^ "))"
 | TyAnnotation(name, ty) -> 
     "(: " ^ name ^ " " ^ string_of_ty ty ^ ")"

let string_of_program deflist = String.concat "\n" (List.map string_of_def deflist)
