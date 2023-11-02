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
     | Ast.Int -> M.Int(32) (* 32-bit integer *)
     | Ast.Bool -> M.Int(1) (* 1-bit integer *)
     | Ast.Unit -> M.Int(1)
     | Ast.Sym -> M.Ptr(Int(8)) (* pointer to a 8-bit integer *)
     | Ast.CustomTy(_) -> raise (Unimplemented "Not implemented")

let convert_typed convert_elem (elem, ty) = (convert_elem elem, convert_ty ty)

let rec convert_expr fast_expr = 
    match fast_expr with 
     | F.Literal(lit) -> M.Literal(lit)
     | F.Local(name) -> M.Local(name)
     | F.Global(name) -> M.Global(name)
     (* NEEDSWORK: Implement case *)
     | F.If(expr1, expr2, expr3) -> 
         M.If(convert_typed convert_expr expr1, convert_typed convert_expr expr2, convert_typed convert_expr expr3)
     | F.Begin(expr1, expr2) -> M.Begin(convert_typed convert_expr expr1, convert_typed convert_expr expr2)
     | F.Let(name, expr, body) -> M.Let(name, convert_typed convert_expr expr, convert_typed convert_expr body)
     | F.Apply(expr, exprlist) -> M.Apply(convert_typed convert_expr expr, List.map (convert_typed convert_expr) exprlist)
     | _ -> raise (Unimplemented "Not implemented")

(* Converts a fast definition to a _list_ of mast definitions *)
let convert_defs fast_def = 
    match fast_def with
     | F.Define("main", params, body) ->
        (* special case: keep "main" as "main" *)
        [ M.Define("main", List.map (convert_typed id) params, convert_typed convert_expr body) ]
     | F.Define(name, params, body) -> 
        (* Prefix each function name with "^" to guarantee it does not conflict with generated functions *)
        [ M.Define("^" ^ name, List.map (convert_typed id) params, convert_typed convert_expr body) ]
     | F.Datatype(name, _) -> (* NEEDSWORK: Actually generate the functions. These are just placeholders for now *)
        [ M.Define("_dup_" ^ name, [("xxs", M.Ptr(M.Int(1)))], (M.Literal(IntLit(0)), M.Int(32))) ;
          M.Define("_free_" ^ name, [("xxs", M.Ptr(M.Int(1)))], (M.Literal(IntLit(0)), M.Int(32))) ]


let mast_of_fast fast = 
    (* Get all datatype definitions, store later *)
    let _ = List.fold_left add_datatype StringMap.empty fast in 
    List.flatten (List.map convert_defs fast)
