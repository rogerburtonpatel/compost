module A = Ast 
module U = Uast
module T = Tast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)
(* type def =
    Define of name * (name list) * expr
  | Datatype of name * (variant list)
  | TyAnnotation of name * ty *)

(* type def =
  Define of name * (name typed) list * expr typed
| Datatype of name * (variant list) *)
exception Impossible of string
exception Todo
exception NotFound of string
exception TypeError of string

(* TODO: test double-bound vcon names (ie. bind cons with let) *)


let rec eqType t1 t2 = match (t1, t2) with 
| (A.Int, A.Int) | (A.Bool, A.Bool) | (A.Sym, A.Sym) | (A.Unit, A.Unit) -> true 
| (A.FunTy (arg_ts, ret_t)), (A.FunTy (arg_ts', ret_t')) -> 
                    eqTypes arg_ts arg_ts' && eqType ret_t ret_t'
| (A.CustomTy n, A.CustomTy n') -> n = n'
| _ -> false
and eqTypes ts ts' = List.equal eqType ts ts'

let isSome = function 
Some _ -> true 
| None -> false 
let isNone x = not (isSome x)

let rec tyString = function 
| A.Int -> "int"
| A.Bool -> "bool"
| A.Sym -> "symbol"
| A.Unit -> "Unit"
| A.FunTy (arg_ts, ret_t) -> 
                    "(-> (" 
                    ^ String.concat " " (List.map tyString arg_ts) ^ ") " 
                    ^ tyString ret_t ^ ")"
| A.CustomTy name -> name 
(* let typesMatchOrError t1 t2 metainfo = 
  if eqType t1 t2 
  then true
  else raise 
      (TypeError ("type mismatch: expected "
                  ^ tyString t1
                  ^ " but got "
                  ^ tyString t2 ^ metainfo)) *)


let checkFunTypes n param_ts arg_ts ret_t = 
  let rec go t_s t_s' = 
  match (t_s, t_s') with 
| ([], []) -> ()
| (tau::taus, tau'::taus') -> 
      if eqType tau tau' 
      then go taus taus'
      else
        let funtysMismatchError n ts ts' ret_t = 
          "type mismatch: expected "
                              ^ tyString (A.FunTy (ts, ret_t))
                              ^ " but got "
                              ^ tyString (A.FunTy (ts', ret_t)) 
                              ^ " in function application of \"" ^ n ^ "\""
        in raise (TypeError (funtysMismatchError n param_ts arg_ts ret_t))
| (_, _) -> raise (Impossible "mismatch in number of types in checkFunArgTypes")
    in go param_ts arg_ts 

let extendGammaWithPat gamma delta pat = 
  match pat with 
  | A.WildcardPattern -> gamma
  | A.Pattern (pn, ns) -> 
    let (typ_args, _) = StringMap.find pn delta in 
    let gamma' = 
      List.fold_left2 (fun g n t -> StringMap.add n t g) 
                        gamma ns typ_args in 
      gamma' 

      (* val transformCase : A.ty -> (A.pattern * U.expr) list -> T.expr *)
(* let transformCase (ty, e) (possibleVariants : Ast.name list) (branches : (T.pattern * T.expr) list) = 
  let checkBranch (newbranches, foundvariants, warn) branch = 
    match branch with 
    | T.WildcardPattern -> 
         (List.append newbranches [branch], possibleVariants, warn)
      | T.Pattern (vcon, ns) -> 
        let warn' = if List.exists (fun vc -> vc = vcon) foundvariants then 
          let (names, tys) = List.split ns in 
          "unreachable pattern \"" ^ vcon
        else ""
        
    (* if List.exists (fun s -> s = branch)
    raise Todo  *)
  in 
  let (newbranches, warn) = List.fold_left checkBranch ([], [], "") branches in 
  let _ = if not (warn = "") then Printf.eprintf ("Warning: %s") warn else () in
  (* todo add wildcard *)
  T.Case (ty, e, newbranches) *)


let curry f x y = f (x, y)

(* gamma: name -> ty *)
(* delta: value constructor name -> (types-of-its-arguments, type-it-constructs) *)

let rec typeof gamma delta expr = 
  let rec typ = function  
  | U.Literal l -> 
    (match l with A.IntLit _ -> A.Int
                              | A.BoolLit _ -> A.Bool
                              | A.SymLit _ -> A.Sym
                              | A.UnitLit -> A.Unit)
  (* NOTE: Do we want a sanity check that all globals are Funty? *)
  | U.Local n | U.Global n -> 
                    if not (StringMap.mem n gamma)
                    then 
                      raise (NotFound ("unbound name \"" ^ n ^ "\""))
                    else StringMap.find n gamma
  | U.If (e1, e2, e3) ->     
    (match (typ e1, typ e2, typ e3) with 
                          | (A.Bool, t1, t2) -> 
                            if t1 = t2 
                            then t1 
                            else raise 
                               (TypeError "mismatched types in if branches")
                          | _ -> raise 
                               (TypeError ("condition failed to typecheck to "
                                          ^ "boolean in \"if\" expression")))
  | U.Let (n, e, e') ->
    let rhs_t = typ e in 
                        let extended_gamma = (StringMap.add n rhs_t gamma) in 
                        typeof extended_gamma delta e'
  | U.Apply (f, es) -> 
    (match f with U.Global n | U.Local n -> 
      if not (StringMap.mem n gamma)
        then raise (NotFound ("attempted to apply unbound name \"" ^ n ^ "\""))
        else let t = StringMap.find n gamma in 
                (match t with A.FunTy (arg_ts, ret_t) -> 
                  let n_expected = List.length arg_ts in 
                  let n_given    = List.length es     in 
                  if n_expected != n_given 
                  then raise (TypeError ("mismatch in number of arguments in " 
                                        ^ "application of function \"" ^ n 
                                        ^ "\": expected "
                                        ^ Int.to_string n_expected 
                                        ^ " but " 
                                        ^ Int.to_string n_given 
                                        ^ " were given."))
                (* typecheck arguments - purely side-effecting *)
                  else 
                    let () = checkFunTypes n (List.map typ es) arg_ts ret_t in 
                  ret_t (* type is return type *)
                | _ -> raise 
                          (TypeError 
                            ("attempted to apply non-function \"" ^ n ^ "\"")))
                            
                | _ -> raise (TypeError "attempted to apply non-function"))
                          
  | U.Dup n -> if not (StringMap.mem n gamma)
    then raise (NotFound ("attempted to dup unbound name \"" ^ n ^ "\""))
    else StringMap.find n gamma

  | U.Case (_, []) -> raise (TypeError "empty case expression")
  | U.Case (e, branches) -> 
    let typ_scrutinee = typ e in 
    (* scrutinee MUST be custom type; no literal pattern matching *)
      (match typ_scrutinee with A.CustomTy sname -> 
        let (patterns, rhss) = List.split branches in 
        (* check all patterns to be well-formed with regards to the scrutinee *)
        let typeCheckPattern = function
          | A.WildcardPattern -> ()
          | A.Pattern (pname, _) -> 
            (* ensure pattern maps to a type *)
            if not (StringMap.mem pname delta)
            then raise (TypeError ("unknown type constructor \"" ^ pname 
                                   ^ "\" in case branch"))
            else 
            (* ensure the type it matches to is correct *)
            let (_, typ_of_pat) = StringMap.find pname delta in 
              if not (eqType typ_scrutinee typ_of_pat)
              then raise (TypeError ("scrutinee in case has type \"" ^ sname 
                                      ^ "\" but a branch is a pattern of type " 
                                      ^ pname ^ " \""))
              else () (* success *)
        in 
        (* typeCheckRHS to be mapped over branches. 
        1. extends rhs environments with pattern-introduced names and types
        2. typechecks rhss
        3. ensure all types are equal *)
        let typeCheckRHS pattern rhs = 
          (* extends gamma with bindings introduced by pat *)
              let extended_gamma = extendGammaWithPat gamma delta pattern in 
              (* print_endline "Typechecking rhs with gamma: \n";
              StringMap.iter (fun s t -> print_endline (s ^ " -> " ^ tyString t)) 
              extended_gamma ; *)
              typeof extended_gamma delta rhs
            in 
            
        (* make bindings over pattern types *)
        let _           = List.iter typeCheckPattern patterns in 
        let typs_rhss   = List.map2 typeCheckRHS patterns rhss in 

        let typ_fst_rhs = List.hd typs_rhss in 
        (* check all rhs's to be of the same type *)
        let check_rhs_ty_match rhs' = 
          if not (eqType rhs' typ_fst_rhs) 
          then raise (TypeError ("a case expression's first branch has type " 
                      ^ tyString typ_fst_rhs ^ ", but a later branch has type "
                      ^ tyString rhs'))
        in 
        let _ = List.iter check_rhs_ty_match typs_rhss in 
        typ_fst_rhs
        | _ -> raise (TypeError
                        ("expected custom datatype in case expression but got " 
                        ^ tyString typ_scrutinee)))
    in typ expr

(* true type inference cookery in the works *)
                    (* and checkApplyAndExtendBindings fun_name (arg_typs, param_names_and_typs) ret_typ gamma delta = 
  let (param_names, param_typs) = List.split param_names_and_typs in 
  match (arg_typs, param_names_and_typs) with 
| ([], []) -> (ret_typ, gamma, delta)
| (None::taus, tau'::taus') ->  *)

(* let rec exp rho = function 
U.Literal l -> T.literal *)

let rec exp gamma delta expr = 
  let typeof' = typeof gamma delta in
  let rec exp' e = 
    match e with 
    | U.Literal l -> T.Literal l 
    | U.Local n   -> let _ = typeof' e in T.Local  n 
    | U.Global n  -> let _ = typeof' e in T.Global n 
    | U.Case (ex, branches) -> 
        let _     = typeof' e in 
        let ty_ex = typeof' ex in 
        let (pats, rhss)  = List.split branches in
        let patconvert    = function 
          | A.WildcardPattern -> T.WildcardPattern
          | A.Pattern (n, ns) -> 
              let (vartys, _) = StringMap.find n delta in 
              let names_tys = List.combine ns vartys in 
              T.Pattern (n, names_tys)
          in 
        let rhs_es = 
          List.map2 (fun pat rhs -> 
                      let extended_gamma = extendGammaWithPat gamma delta pat 
                      in exp extended_gamma delta rhs) pats rhss in 
        let pats'         = List.map patconvert pats in 
        let branches'     = List.combine pats' rhs_es in 
        (* let branches'     = List.map (fun (pat, (e, t)) -> 
                                           T.CaseBranch (pat, (e, t))) 
                            branches_full in  *)
        T.Case (ty_ex, exp' ex, branches')
    | U.If (e1, e2, e3) -> 
        let _ = typeof' e in
          T.If (exp' e1, exp' e2, exp' e3)
    | U.Let (n, e1, e') ->  let ty_e   = typeof' e1 in 
                            let gamma' = StringMap.add n ty_e gamma in 
                            let _      = typeof gamma' delta e' in
                            T.Let (n, ty_e, exp gamma delta e1, 
                                            exp gamma' delta e')
    | U.Apply (e, es) as app -> let _ = typeof' app in 
                                  let es' = List.map exp' es in 
                                  T.Apply (exp' e, es')
   | U.Dup n -> let ty = typeof' e in T.Dup (ty, n)
  in exp' expr

let typecheckDef (defs, gamma, delta) = function
| U.Define (n, args, body) -> 
  if not (StringMap.mem n gamma)
  then raise (TypeError 
                ("definition of function \"" ^ n 
                ^ "\" with no prior type annotation."))
  else let known_annotated_ty = StringMap.find n gamma in
  (match known_annotated_ty with 
    | (A.FunTy (argtys, expected_ret_ty)) -> 
      let known_argscount = List.length argtys in 
      let given_argscount = List.length args in 
      if not (known_argscount = given_argscount)
      then raise (TypeError ("prior annotation defined function \"" 
                             ^ n ^ "\" has " ^ Int.to_string known_argscount 
                             ^ " arguments, but its definition has "
                             ^ Int.to_string given_argscount ^ " arguments."))
      else 
      let extended_gamma = 
        List.fold_left2 (* insane folding *)
            (fun env name ty -> StringMap.add name ty env) gamma args argtys in 
      let ret_ty = typeof extended_gamma delta body in 
        if not (eqType expected_ret_ty ret_ty) 
        then raise (TypeError ("prior annotation defined function \"" 
                              ^ n ^ "\" to be of type \"" 
                              ^ tyString known_annotated_ty
                              ^ "\" but a definition was given that has type \""
                              ^ tyString (A.FunTy (argtys, ret_ty )) ^ "\""))
      else 
        let funty = Ast.FunTy (argtys, ret_ty) in 
        let def' = T.Define (n, funty, args, exp extended_gamma delta body) in
        (List.append defs [def'], gamma, delta)
    | _ -> raise (Impossible "found non-func name in top-level environment"))
| U.Datatype (n, variants) -> 
  let check_variant delta' (vname, ts)  =
    if not (StringMap.mem vname delta')
    then (StringMap.add vname (ts, A.CustomTy n) delta') 
    else 
      let (_, existing_type) = StringMap.find vname delta in 
      raise (TypeError ("duplicate type constructor \"" 
                        ^ vname ^ "\" in user-defined datatype \""
                        ^ n ^ "\": constructor already exists for type \"" 
                        ^ tyString existing_type ^ "\"")) 
    in let extended_delta = List.fold_left check_variant delta variants in
    let add_variant gamma' (vname, ts) = 
      StringMap.add vname (Ast.FunTy (ts, A.CustomTy n)) gamma' 
    in 
    let extended_gamma = List.fold_left add_variant gamma variants in 
    let datatype' = T.Datatype (n, variants) in
      List.append defs [datatype'], extended_gamma, extended_delta

| U.TyAnnotation (n, ty) -> 
  if not (StringMap.mem n gamma)
  then let extended_gamma = StringMap.add n ty gamma in 
  (defs, extended_gamma, delta)
  else let found_typ = StringMap.find n gamma in 
    if not (eqType ty found_typ)
    then raise (TypeError ("prior annotation defined function \"" ^ n ^ 
    "\" to be of type \"" 
    ^ tyString found_typ
    ^ "\" but a second annotation was given that has type \""
    ^ tyString ty ^ "\""))

else (defs, gamma, delta)
(* walks the program, building environments and typechecking against them. *)
let typecheck prog =
  let gamma =
    let prim_constraints = List.fold_right
        (fun (prim_name, ty) -> StringMap.add prim_name ty)
        Primitives.primitives StringMap.empty
    in
    let fun_constraints = List.fold_right
        (function
          | U.TyAnnotation (n, ty) -> StringMap.add n ty
          | _ -> fun env -> env)
        prog StringMap.empty
    in
    StringMap.union
      (fun n _ _ -> raise 
                      (TypeError ("attempted to define a function with "
                                  ^ "name \"" ^ n ^ "\", but a primitive "
                                  ^ "function with that name already exists.")))
      prim_constraints fun_constraints
  in
  let delta = StringMap.empty in
  let defs = [] in 
  List.fold_left typecheckDef (defs, gamma, delta) prog
