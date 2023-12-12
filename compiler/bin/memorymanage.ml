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
exception Impossible of string 

(* Convert Compost type `ty` to the appropriate LLVM type *)
let rec convert_builtin_ty ty = 
    match ty with 
    | Uast.FunTy(tylist, ty) ->
        let convert_param_ty = function
            | Uast.FunTy _  as fun_ty -> M.Ptr(convert_builtin_ty fun_ty)
            | other_ty -> convert_builtin_ty other_ty
        in
        M.Fun(convert_builtin_ty ty, List.map convert_param_ty tylist)
    | Uast.Unit -> M.Int(1)
    | Uast.Int -> M.Int(32) (* 32-bit integer *)
    | Uast.Bool -> M.Int(1) (* 1-bit integer *)
    | Uast.Sym -> M.Ptr(Int(8)) (* pointer to a 8-bit integer *)
    | Uast.CustomTy(_) -> raise (Impossible "Erroneously called convert_builtin_ty on a custom datatype")

(* Convert an integer to a generated variable name corresponding to that integer, e.g. 1 -> "var1" *)
let varname_of_int int = "var" ^ string_of_int int 

let mast_of_fast fast = 
    (* Get all datatype definitions *)
    let datatypes = 
        let add_datatype map def = 
            match def with 
            | F.Datatype(name, variants) -> StringMap.add name variants map 
            | _ -> map
        in
        List.fold_left add_datatype StringMap.empty fast 
    in 
    (* Get all variant constructor definitions and generate indices *)
    let variant_tags = 
        let add_variant (map, tag) (name, _) = (StringMap.add name tag map, tag + 1) in 
        let add_datatype_variant map def = 
            match def with 
            | F.Datatype(_, variants) -> 
                let (map', _) = List.fold_left add_variant (map, 0) variants in 
                map'
            | _ -> map (* No change if not a datatype definition *) 
        in
        List.fold_left add_datatype_variant StringMap.empty fast 
    in
    (* Convert fast ty to mast ty *)
    let rec convert_ty ty = 
        match ty with 
        (* LLVM type for a custom type is a pointer to a struct containing a 
         * 32-bit variant identifier (i.e. tag), followed by $n$ i64s, where 
         * $n$ equals the maximum number of arguments for a variant constructor 
         * of this type
         *)
        | Uast.CustomTy(tyname) -> 
            let variants = StringMap.find tyname datatypes in
            (* Get maxnum_variantargs, which is the maximum possible number of arguments to 
             * a variant constructor of this type 
             *)
            let update_maxnum_variantargs currmax currvariant = 
                let (_, currvarianttys) = currvariant in 
                max currmax (List.length currvarianttys) 
            in
            let maxnum_variantargs = List.fold_left update_maxnum_variantargs 0 variants in 
            M.Ptr(M.Struct(M.Int(32) :: List.init maxnum_variantargs (fun _ -> M.Int(64))))
        | Uast.FunTy(tylist, ty) ->
            let convert_param_ty = function
                | Uast.FunTy _  as fun_ty -> M.Ptr(convert_ty fun_ty)
                | other_ty -> convert_ty other_ty
            in
            M.Fun(convert_ty ty, List.map convert_param_ty tylist)
        | _ -> convert_builtin_ty ty 
    in 
    (* Convert fast expr to mast expr *)
    let rec convert_expr fast_expr =
        match fast_expr with
        | F.Literal(lit) -> M.Literal(lit)
        | F.Local(name) -> M.Local(name)
        | F.Global("main") -> M.Global("main")
        | F.Global(name) when List.mem_assoc name Primitives.primitives -> M.Global(name)
        | F.Global(name) -> M.Global("_" ^ name)
        | F.Case(expr, casebranches) ->
            let convert_casebranch (pattern, pexpr) = 
                let convert_pattern pattern = 
                    match pattern with 
                    | F.Pattern(name, names) ->
                        M.Pattern(StringMap.find name variant_tags, List.map (fun (name, ty) -> (name, convert_ty ty)) names)
                    | F.Name n -> M.Name n
                in
                (convert_pattern pattern, convert_expr pexpr) 
            in 
            M.Case(convert_expr expr, List.map convert_casebranch casebranches)
        | F.If(expr1, expr2, expr3) ->
            M.If(convert_expr expr1, convert_expr expr2, convert_expr expr3)
        | F.Let(name, expr, body) -> M.Let(name, convert_expr expr, convert_expr body)
        | F.Apply(expr, exprlist) -> M.Apply(convert_expr expr, List.map convert_expr exprlist)
        | F.Dup(ty, name) -> 
            (match ty with 
              | CustomTy(tyname) -> M.Apply(M.Global("dup_" ^ tyname), [M.Local(name)])
              | _ -> M.Local(name) (* no-op for now that returns the name; another option is to throw InvalidDup exception *))
        | F.FreeRec(ty, name, expr) -> 
            (match ty with 
              | CustomTy(tyname) -> M.Let(Freshnames.fresh_name (), M.Apply(M.Global("free_" ^ tyname), [M.Local(name)]), convert_expr expr)
              | _ -> raise (Impossible "Erroneously called FreeRec on something that was not a custom type"))
        | F.Free(_, name, expr) -> M.Free(name, convert_expr expr)
        | F.Err (ty, msg) -> M.Err (convert_ty ty, msg)
    in
    (* Converts a fast definition to a _list_ of mast definitions *)
    let convert_defs fast_def =
        match fast_def with
        | F.Define("main", fun_ty, params, body) ->
            [ M.Define("main", convert_ty fun_ty, params, convert_expr body) ]
        | F.Define(name, fun_ty, params, body) ->
            (* Prefix each function name with "_" to guarantee it does not conflict with generated functions *)
            [ M.Define("_" ^ name, convert_ty fun_ty, params, convert_expr body) ]
        | F.Datatype(name, variants) ->
            (* Generate all relevant functions for this datatype *)
            let data_ty = match convert_ty (Uast.CustomTy(name)) with
              | M.Ptr ty -> ty
              | _ -> raise (Impossible "custom type is not pointer to struct")
            in
            let data_ty_ptr = M.Ptr data_ty in
            let dup_func = 
                let func_type = M.Fun(data_ty_ptr, [data_ty_ptr]) in
                let param_names = ["instance"] in 
                let body = 
                    let gen_casebranch variant_idx (_, variant_tys) = 
                        (* Let variant_varnames just be a sequence of integers starting from 0, prepended by "var" *)
                        let variant_varnames = List.init (List.length variant_tys) varname_of_int in 
                        let pattern = M.Pattern(variant_idx, List.combine variant_varnames (List.map convert_ty variant_tys)) in
                        let expr = 
                            let alloc_ty index ty = 
                            match ty with 
                             | Uast.CustomTy(name) -> (index + 1, M.Apply(Global("dup_" ^ name), [Local(varname_of_int index)]))
                             | _ -> (index + 1, Local(varname_of_int index)) 
                            in 
                            let (_, alloc_expr) = List.fold_left_map alloc_ty 0 variant_tys 
                            in 
                            M.Alloc(data_ty, variant_idx, alloc_expr) 
                        in 
                        (variant_idx + 1, (pattern, expr))
                    in 
                    let (_, casebranches) = List.fold_left_map gen_casebranch 0 variants in
                    let casebranches' = List.append casebranches [(M.Name (Freshnames.fresh_name ()), M.Err (data_ty_ptr, "IMPOSSIBLE: inexhaustive match in dup"))] in
                    M.Case(Local("instance"), casebranches')
                in 
                M.Define("dup_" ^ name, func_type, param_names, body)
            in 
            let free_func = 
                let func_type = M.Fun(convert_ty Uast.Unit, [data_ty_ptr]) in
                let param_names = ["instance"] in 
                let body = 
                    let gen_casebranch variant_idx (_, variant_tys) = 
                        (* Let variant_varnames just be a sequence of integers starting from 0, prepended by "var" *)
                        let variant_varnames = List.init (List.length variant_tys) varname_of_int in 
                        let pattern = M.Pattern(variant_idx, List.combine variant_varnames (List.map convert_ty variant_tys)) in
                        let expr = 
                            let unitlit = M.Literal(Ast.UnitLit) in 
                            let gen_free_call varname ty = 
                                match ty with 
                                 | Uast.CustomTy(name) -> M.Apply(Global("free_" ^ name), [Local(varname)]) 
                                 | _ -> unitlit 
                            in 
                            (* Get all calls to free functions *)
                            let free_calls = List.filter ((<>) unitlit) (List.map2 gen_free_call variant_varnames variant_tys) in 
                            let rec free_expr_of_calls free_calls = 
                                match free_calls with 
                                 | [] -> unitlit
                                 | [call] -> call 
                                 | call :: calls -> M.Let(Freshnames.fresh_name (), call, free_expr_of_calls calls)
                            in 
                            M.Free("instance", free_expr_of_calls free_calls)
                        in 
                        (variant_idx + 1, (pattern, expr))
                    in 
                    let (_, casebranches) = List.fold_left_map gen_casebranch 0 variants  in
                    let casebranches' = List.append casebranches [(M.Name (Freshnames.fresh_name ()), M.Err (M.Int 1, "IMPOSSIBLE: inexhaustive match in dup"))] in
                    M.Case(Local("instance"), casebranches')
                in 
                M.Define("free_" ^ name, func_type, param_names, body) 
            in
            let alloc_variant_funcs = 
                let alloc_variant_func (variant_name, variant_tys) = 
                    (* Let argument names of function just be a sequence of integers starting from 0, prepended by "var" *)
                    let func_type = M.Fun(data_ty_ptr, (List.map convert_ty variant_tys)) in
                    let func_argnames = List.init (List.length variant_tys) varname_of_int in 
                    let alloc_ty index _ = (index + 1, M.Local(varname_of_int index)) in 
                    let (_, alloc_expr) = List.fold_left_map alloc_ty 0 variant_tys in 
                    let alloc_call = M.Alloc(data_ty, StringMap.find variant_name variant_tags, alloc_expr) in 
                    M.Define("_" ^ variant_name, func_type, func_argnames, alloc_call)
                in
                let variants = StringMap.find name datatypes in 
                List.map alloc_variant_func variants 
            in
            dup_func :: free_func :: alloc_variant_funcs
    in 
    let defs = List.map convert_defs fast in 
    List.flatten defs
