(* Unambiguous Abstract Syntax Tree *)
type name = string

type ty = Ast.ty

type literal = Ast.literal

type pattern = Ast.pattern

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of expr * (pattern * expr) list
  | If of expr * expr * expr
  | Let of name * expr * expr
  | Apply of expr * (expr list)
  | Dup of name

type def =
    Define of name * name list * expr
  | Datatype of name * (name * ty list) list
  | TyAnnotation of name * ty

type program = def list


(* Backwards to PAst & Printing *)

let up_to_pp = function
    Ast.Pattern(cn, ns) -> Ast.Pattern(cn, List.map ((^) "%") ns)
  | Ast.WildcardPattern -> Ast.WildcardPattern

let rec ucb_to_pcb = function (p, expr) -> (up_to_pp p, uexpr_to_pexpr expr)

and uexpr_to_pexpr = function
    Literal(lit) -> Past.Literal(lit)
  | Local(name) -> Past.NameExpr("%" ^ name)
  | Global(name) -> Past.NameExpr("@" ^ name)
  | If(expr1, expr2, expr3) -> Past.If(uexpr_to_pexpr expr1, uexpr_to_pexpr expr2, uexpr_to_pexpr expr3)
  | Let(name, expr1, expr2) -> Past.Let("%" ^ name, uexpr_to_pexpr expr1, uexpr_to_pexpr expr2)
  | Apply(expr, exprlist) -> Past.Apply(uexpr_to_pexpr expr, (List.map uexpr_to_pexpr exprlist))
  | Case(expr, casebranchlist) -> Past.Case(uexpr_to_pexpr expr, (List.map ucb_to_pcb casebranchlist))
  | Dup(name) -> Past.Dup(name)

let udef_to_pdef = function
    Define(name, namelist, expr) -> Past.Define("@" ^ name, namelist, uexpr_to_pexpr expr)
  | Datatype(name, variantlist) -> Past.Datatype(name, variantlist)
  | TyAnnotation(name, ty) -> Past.TyAnnotation("@" ^ name, ty)

let past_of_program deflist = List.map udef_to_pdef deflist

let string_of_program deflist = Past.string_of_program (past_of_program deflist)
