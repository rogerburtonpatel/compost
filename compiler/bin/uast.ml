(* Unambiguous Abstract Syntax Tree *)
type name = string

type filename = name (* NEEDSWORK: Decide if we want different rules for filenames *)

type ty = Ast.ty

type literal = Ast.literal

type pattern = Ast.pattern

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of expr * (pattern * expr) list
  | If of expr * expr * expr
  | Begin of expr * expr
  | Let of name * expr * expr
  | Apply of expr * (expr list)
  | Dup of name

type def =
    Val of name * expr
  | Define of name * name list * expr
  | Datatype of name * (name * ty list) list
  | TyAnnotation of name * ty

type program = def list

(* Pretty printing functions *)

let rec string_of_namelist = function
   [] -> ""
 | name :: names -> name ^ " " ^ string_of_namelist names

let string_of_ty = Ast.string_of_ty

let string_of_symlit lit =
   let escape_backslashes = String.concat "\\\\" (String.split_on_char '\\' lit)
   in
   let escape_quotes = String.concat "\\\'" (String.split_on_char '\'' escape_backslashes)
   in
   "'" ^ escape_quotes ^ "'"

let string_of_lit = Ast.string_of_lit

let is_int = String.for_all (function '0' .. '1' -> true | _ -> false)

let string_of_nameorwildcard = function
   name when is_int name -> "_"
 | name -> name

let string_of_pattern = Ast.string_of_pattern

let string_of_variant = function
   (name, tylist) -> "[" ^ name ^ " (" ^ String.concat " " (List.map string_of_ty tylist) ^ ")]"

let rec string_of_expr = function
   Literal(lit) -> string_of_lit lit
 | Local(name) -> "%" ^ name
 | Global(name) -> "@" ^ name
 | If(expr1, expr2, expr3) ->
     "(if " ^ string_of_expr expr1 ^ " " ^ string_of_expr expr2 ^ " " ^ string_of_expr expr3 ^ ")"
 | Begin(expr1, expr2) ->
     "(begin " ^ string_of_expr expr1 ^ " " ^ string_of_expr expr2 ^ ")"
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
