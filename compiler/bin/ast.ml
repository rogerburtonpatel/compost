(* Abstract Syntax Tree and functions for printing it *)

type name = string 

type ty = Int | Char | Bool | Unit | Sym | CustomTy of name 

type literal = 
    IntLit of int 
  | BoolLit of bool 
  | SymLit of string 
  | UnitLit 

type nameorwildcard = 
    PatternBindVar of name 
  | WildcardBind

type pattern = 
    Pattern of name * nameorwildcard list 
  | WildcardPattern

type casebranch = CaseBranch of pattern * expr 

and bind = name * expr 

and expr = 
    Literal of literal 
  | NameExpr of name 
  | Case of expr * (casebranch list) 
  | If of expr * expr * expr 
  | Begin of expr list 
  | Let of (bind list) * expr 
  | Apply of expr * (expr list) 

type variant = Variant of name * (ty list) 

type def = 
    Val of name * expr
  | Define of name * (name list) * expr 
  | Datatype of name * (variant list) 
  | TyAnnotation of name * ty 
  (* NEEDSWORK: Determine if we're allowing "use" on filename *)

type program = def list 

(* Pretty printing functions *)

let rec string_of_namelist = function 
   [] -> ""
 | name :: names -> name ^ " " ^ string_of_namelist names 

let string_of_lit = function 
   IntLit(lit) -> string_of_int lit 
 | BoolLit(lit) -> string_of_bool lit 
 | SymLit(lit) -> "'" ^ lit ^ "'"
 | UnitLit -> "unit"

let string_of_nameorwildcard = function 
   PatternBindVar(name) -> name 
 | WildcardBind -> "_"

let string_of_pattern = function 
   Pattern(name, nameorwildcardlist) -> 
     "(" ^ name ^ " " ^ String.concat " " (List.map string_of_nameorwildcard nameorwildcardlist) ^ ")"
 | WildcardPattern -> "_"

let rec string_of_expr = function 
   Literal(lit) -> string_of_lit lit 
 | NameExpr(name) -> name 
 | If(expr1, expr2, expr3) -> 
     "(if " ^ string_of_expr expr1 ^ " " ^ string_of_expr expr2 ^ " " ^ string_of_expr expr3 ^ ")"
 | Begin(exprlist) -> 
     "(begin " ^ String.concat " " (List.map string_of_expr exprlist) ^ ")"
 | Let(bindlist, expr) ->
     "(let " ^ "(" ^ String.concat " " (List.map string_of_bind bindlist) ^ ") " ^ string_of_expr expr ^ ")"
 | Apply(expr, exprlist) -> 
     "(" ^ string_of_expr expr ^ " " ^ String.concat " " (List.map string_of_expr exprlist) ^ ")"
 | Case(expr, casebranchlist) ->
     "(case " ^ string_of_expr expr ^ " (" ^ String.concat " " (List.map string_of_casebranch casebranchlist) ^ "))"

and string_of_bind = function 
   (name, expr) -> "[" ^ name ^ " " ^ string_of_expr expr ^ "]"
  
and string_of_casebranch = function 
   CaseBranch(pattern, expr) -> "[" ^ string_of_pattern pattern ^ " " ^ string_of_expr expr ^ "]"

let string_of_def = function 
   Val(name, expr) -> "(val " ^ name ^ " " ^ string_of_expr expr ^ ")"
 | Define(name, namelist, expr) -> 
     "(define " ^ name ^ " (" ^ String.concat " " namelist ^ ") " ^ string_of_expr expr ^ ")"
 | _ ->
     "unimplemented"
 (* NEEDSWORK: Implement pretty printing for other defines *)

let string_of_program deflist = String.concat "\n" (List.map string_of_def deflist)
