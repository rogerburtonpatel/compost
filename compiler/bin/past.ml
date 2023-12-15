(* Processed Abstract Syntax Tree and functions for printing it *)

(* Author: Jackson Warhover 
 * Edited by: Jasper Geer, Roger Burtonpatel 
 *)

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
    Define of name * (name list) * expr 
  | Datatype of name * (name * ty list) list
  | TyAnnotation of name * ty 

type program = def list


(* Backwards to Ast & Printing *)

let rec pcb_to_acb = function (p, expr) -> (p, pexpr_to_aexpr expr)

and pexpr_to_aexpr = function
    Literal(lit) -> Ast.Literal(lit)
  | NameExpr(name) -> Ast.NameExpr(name)
  | If(expr1, expr2, expr3) -> Ast.If(pexpr_to_aexpr expr1, pexpr_to_aexpr expr2, pexpr_to_aexpr expr3)
  | Let(name, expr1, expr2) -> Ast.Let([(name, pexpr_to_aexpr expr1)], pexpr_to_aexpr expr2)
  | Apply(expr, exprlist) -> Ast.Apply(pexpr_to_aexpr expr, (List.map pexpr_to_aexpr exprlist))
  | Case(expr, casebranchlist) -> Ast.Case(pexpr_to_aexpr expr, (List.map pcb_to_acb casebranchlist))
  | Dup(name) -> Ast.Dup(name)

let pdef_to_adef = function
    Define(name, namelist, expr) -> Ast.Define(name, namelist, pexpr_to_aexpr expr)
  | Datatype(name, variantlist) -> Ast.Datatype(name, variantlist)
  | TyAnnotation(name, ty) -> Ast.TyAnnotation(name, ty)

let ast_of_program deflist = List.map pdef_to_adef deflist

let string_of_program deflist = Ast.string_of_program (ast_of_program deflist)
