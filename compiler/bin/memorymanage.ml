(* Converts explicit free AST to explicit memory managed AST *)

(* 
 * Notes: Convert types to LLVM types 
 * Generate alloc and free functions for each datatype 
 * Convert all dup calls to the appropriate alloc function (and 
 * use the "primitive" alloc where necessary)
 * Convert all freerec calls to the appropriate free function 
 * Keep all free calls the way they are 
 * Delete datatype definitions 
 * Prefix user functions with something other than "_" (probably "^")
 *)

module F = Fast 
module M = Mast 

module StringMap = Map.Make(String)

let typeof (_, ty) = ty

let id x = x 

(* Defined in case we want to throw an error *)
exception InvalidDup of string 
exception Unimplemented of string 

let add_datatype map def = 
    match def with 
     | F.Datatype(name, variants) -> StringMap.add name variants map 
     | _ -> map

let rec convert_ty(ty) = 
    match ty with 
     | Ast.FunTy(tylist, ty) -> M.Fun(convert_ty ty, List.map convert_ty tylist)
     | Ast.Unit -> M.Int(1)
     | Ast.Int -> M.Int(32) (* 32-bit integer *)
     | Ast.Bool -> M.Int(1) (* 1-bit integer *)
     | Ast.Sym -> M.Ptr(Int(8)) (* pointer to a 8-bit integer *)
     | Ast.CustomTy(_) -> raise (Unimplemented "Not implemented")

let rec convert_expr fast_expr =
    match fast_expr with
     | F.Literal(lit) -> M.Literal(lit)
     | F.Local(name) -> M.Local(name)
     | F.Global("main") -> M.Global("main")
     | F.Global(name) when List.mem_assoc name Primitives.primitives -> M.Global(name)
     | F.Global(name) -> M.Global("_" ^ name)
     (* NEEDSWORK: Implement case *)
     | F.If(expr1, expr2, expr3) ->
         M.If(convert_expr expr1, convert_expr expr2, convert_expr expr3)
     | F.Begin(expr1, expr2) -> M.Begin(convert_expr expr1, convert_expr expr2)
     | F.Let(name, expr, body) -> M.Let(name, convert_expr expr, convert_expr body)
     | F.Apply(expr, exprlist) -> M.Apply(convert_expr expr, List.map (convert_expr) exprlist)
     | _ -> raise (Unimplemented "Not implemented")

(* Converts a fast definition to a _list_ of mast definitions *)
let convert_defs fast_def =
    match fast_def with
     | F.Define("main", fun_ty, params, body) ->
         [ M.Define("main", convert_ty fun_ty, params, convert_expr body) ]
     | F.Define(name, fun_ty, params, body) ->
         (* Prefix each function name with "^" to guarantee it does not conflict with generated functions *)
         [ M.Define("_" ^ name, convert_ty fun_ty, params, convert_expr body) ]
     | F.Datatype(name, _) -> raise (Unimplemented "datatypes not implemented")


let mast_of_fast fast = 
    (* Get all datatype definitions, store later *)
    let _ = List.fold_left add_datatype StringMap.empty fast in 
    List.flatten (List.map convert_defs fast)
