module L = Llvm
module M = Mast
module P = Primitives

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

  let symbols =
    let rec get_sym_lits = function
      | M.Literal (Ast.SymLit str) -> StringSet.singleton str
      | M.Case (_, e, branches) ->
        let branch_lits = unions (List.map (fun (_, e) -> get_sym_lits e) branches) in
        StringSet.union (get_sym_lits e) branch_lits
      | M.If (e1, e2, e3) -> unions [get_sym_lits e1; get_sym_lits e2; get_sym_lits e3]
      | M.Begin (e1, e2) -> unions [get_sym_lits e1; get_sym_lits e2]
      | M.Let (n, e1, e2) -> unions [get_sym_lits e1; get_sym_lits e2]
      | M.Apply (e, args) -> unions ((get_sym_lits e) :: (List.map get_sym_lits args))
      | M.Free (_, e) -> get_sym_lits e
      | _ -> StringSet.empty
    in
    let sym_lits = unions (List.map (fun (M.Define (_, _, _, body)) -> get_sym_lits body) program) in
    let build_symbol sym_lit syms =
      let sym_value = L.const_string context sym_lit in
      let sym_var = L.define_global sym_lit sym_value the_module in
      StringMap.add sym_lit sym_var syms
    in
    StringSet.fold build_symbol sym_lits StringMap.empty
  in

  (* Association list of primitive functions names and how to build them *)
  let primitives =
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in
    let unit_value = L.const_int i1_t 0 in
    [
      ("print-sym", fun v b ->
          let _ = L.build_call printf_func v "tmp" b in
          L.build_ret unit_value b
      );
      ("print-int", fun v b ->
          let fmt_int = L.build_global_stringptr "fmt_int" "%d" b in
          let args = Array.append (Array.make 1 fmt_int) v in
          let _ = L.build_call printf_func args "tmp" b in
          L.build_ret unit_value b
      )
    ]
  in

  (* Build the set of function declarations *)
  let functions =
    let primitive_decls =
      let build_primitive (fun_name, build_fun) decls =
        let fun_ty = List.assoc fun_name P.primitives in
        let fun_lltype = lltype_of_ty (Memorymanage.convert_ty fun_ty) in
        let decl = L.define_function fun_name fun_lltype the_module in
        let builder = L.builder_at_end context (L.entry_block decl) in
        let _ = build_fun (L.params decl) builder in
        StringMap.add fun_name decl decls
      in
      List.fold_right build_primitive primitives StringMap.empty
    in
    let function_decl (M.Define (fun_name, fun_ty, _, _)) decls =
      let fun_lltype = lltype_of_ty fun_ty in
      StringMap.add fun_name (L.define_function fun_name fun_lltype the_module) decls
    in
    let decls = List.fold_right function_decl program StringMap.empty in
    let name_conflict = Impossible "user-defined function and primitive function share the same name" in
    StringMap.union (fun _ _ -> raise name_conflict) decls primitive_decls
  in
  let rec expr builder locals = function
    | M.Literal l -> begin match l with
      | Ast.IntLit i -> L.const_int i32_t i
      | Ast.SymLit s ->
        let sym = StringMap.find s symbols in
        L.build_bitcast sym (L.pointer_type i8_t) "tmp" builder
      | Ast.BoolLit b -> if b
        then L.const_int i1_t 1
        else L.const_int i1_t 0
      | Ast.UnitLit -> L.const_int i1_t 0
      end
    | M.Local n -> begin try StringMap.find n locals with _ -> raise (Impossible n) end
    | M.Global n -> begin try StringMap.find n functions with _ -> raise (Impossible n) end
    | M.Begin (e1, e2) ->
      let _ = expr builder locals e1 in
      expr builder locals e2
    | M.Let (n, e, body) ->
      let locals' = StringMap.add n (expr builder locals e) locals in
      expr builder locals' body
    | M.Apply (f, args) ->
      let f_val = expr builder locals f in
      let args' = List.map (expr builder locals) args in
      L.build_call f_val (Array.of_list args') "tmp" builder
    | M.Free (n, e) ->
      let _ = L.build_free (StringMap.find n locals) builder in
      expr builder locals e
      (* TODO Alloc *)
      (* TODO If *)
      (* TODO Case *)
    | _ -> raise (Impossible "Unimplemented")
  in
  let build_function_body (M.Define (n, _, params, body)) =
    let the_function = StringMap.find n functions in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let init_locals =
      let param_values = L.params the_function in
      let bindings = List.mapi (fun i n -> (n, Array.get param_values i)) params in
      List.fold_right (fun (n, v) m -> StringMap.add n v m) bindings StringMap.empty
    in
    let body_value = expr builder init_locals body in
    let _ = L.build_ret body_value builder in
    ()
  in
  List.iter build_function_body program;
  the_module
