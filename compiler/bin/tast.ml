(* Type-Checked Abstract Syntax Tree *)

(* Author: Jasper Geer 
 * Edited by: Roger Burtonpatel, Randy Dang, Jackson Warhover 
 *)

(* open Ast *)

type name = Ast.name

type filename = Ast.filename

type ty = Uast.ty

type 'a typed = 'a * ty

type literal = Ast.literal

type pattern =
    Pattern of name * (name typed) list
  | Name of name * bool

and expr =
    Literal of literal
  | Local of name
  | Global of name
  | Case of (expr typed) * (pattern * (expr typed)) list
  | If of (expr typed) * (expr typed) * (expr typed)
  | Let of name * (expr typed) * (expr typed)
  | Apply of (expr typed) * (expr typed) list
  | Dup of name
  | Err of string

type def =
    Define of name * ty * name list * (expr typed)
  | Datatype of name * (name * ty list) list

type program = def list
