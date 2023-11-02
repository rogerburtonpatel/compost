module L = Llvm
module M = Mast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let ctx = L.create_context

exception Impossible of string

let codegen program =
  let context = L.global_context () in
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and the_module = L.create_module context "Compost" in

  let rec lltype_of_ty = function
    | M.Int i -> L.integer_type context i
    | M.Fun (ret_ty, param_tys) ->
      let param_tys' = List.map lltype_of_ty param_tys in
      L.function_type (lltype_of_ty ret_ty) (Array.of_list param_tys')
    | M.Struct tys ->
      let tys' = List.map lltype_of_ty tys in
      L.struct_type context (Array.of_list tys')
    | M.Ptr ty -> L.pointer_type (lltype_of_ty ty)
  in

  let unions sets = List.fold_right StringSet.union sets StringSet.empty in

  (* let symbols = *)
  (*   let rec get_sym_lits = function *)
  (*     | M.Literal (Ast.SymLit str) -> StringSet.singleton str *)
  (*     | M.Case (_, branches) -> unions (List.map (fun (M.CaseBranch (_, (e, _))) -> get_sym_lits e) branches) *)
  (*     | M.If ((e1, _), (e2, _), (e3, _)) -> unions [get_sym_lits e1; get_strings e2; get_strings e3] *)
  (*     | M.Begin ((e1, _), (e2, _)) -> unions [get_sym_lits e1; get_strings e2] *)
  (*     | M.Let (n, (e1, _), (e2, _)) -> unions [get_sym_lits e1; get_strings e2] *)
  (*     | M.Apply ((e, _), args) -> unions ((get_sym_lits e) :: (List.map (fun (arg, _) -> get_strings arg) args)) *)
  (*     | M.Free (_, _, (e, _)) -> get_sym_lits e *)
  (*     | _ -> StringSet.empty *)
  (*   in *)
  (*   let sym_lits = unions (List.map (fun (M.Define (_, _, (body, _))) -> get_sym_lits body) program) in *)
  (*   let build_symbol sym_lit syms = *)
  (*     let sym = L.build_global_stringptr sym_lit ("str_" ^ sym_lit) in *)


  (* in *)

  let functions =
    let function_decl (M.Define (n, params, (_ , return_ty))) defs =
      let formal_types = Array.of_list (List.map (fun (_, ty) -> lltype_of_ty ty) params) in
      let return_ty' = lltype_of_ty return_ty in
      let function_ty = L.function_type return_ty' formal_types in
      StringMap.add n (L.define_function n function_ty the_module) defs
    in
    List.fold_right function_decl program StringMap.empty
  in

  let rec expr builder locals = function
    | M.Literal l -> begin match l with
      | Ast.IntLit i -> L.const_int i32_t i
      | Ast.SymLit s -> L.build_global_stringptr s ("sym_" ^ s) builder
      | Ast.BoolLit b -> if b
        then L.const_int i1_t 1
        else L.const_int i1_t 0
      | Ast.UnitLit -> L.const_int i1_t 0
      end
    | M.Local n -> StringMap.find n locals
    | M.Global n -> StringMap.find n functions
    | M.Begin ((e1, _), (e2, _)) ->
      let _ = expr builder locals e1 in
      expr builder locals e2
    | M.Let (n, (e, _), (body, _)) ->
      let locals' = StringMap.add n (expr builder locals e) locals in
      expr builder locals' body
    | M.Apply ((f, _), args) ->
      let f' = expr builder locals f in
      let args' = List.map (fun (arg, _) -> expr builder locals arg) args in
      L.build_call f' (Array.of_list args') "tmp" builder
    | M.Free (_, n, (e, _)) ->
      let _ = L.build_free (StringMap.find n locals) builder in
      expr builder locals e
      (* TODO Alloc *)
      (* TODO If *)
    | _ -> raise (Impossible "Unimplemented")

  in

  let build_function_body (M.Define (n, params, (body, _))) =
    let the_function = StringMap.find n functions in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let init_locals =
      let param_values = L.params the_function in
      let bindings = List.mapi (fun i (n, _) -> (n, Array.get param_values i)) params in
      List.fold_right (fun (n, v) m -> StringMap.add n v m) bindings StringMap.empty
    in

    let body_value = expr builder init_locals body in
    let _ = L.build_ret body_value builder in
    ()
  in

  List.iter build_function_body program;
  the_module
