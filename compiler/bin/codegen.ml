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
      | M.Let (_, e1, e2) -> unions [get_sym_lits e1; get_sym_lits e2]
      | M.Apply (e, args) -> unions ((get_sym_lits e) :: (List.map get_sym_lits args))
      | M.Free (_, e) -> get_sym_lits e
      | _ -> StringSet.empty
    in
    let sym_lits = unions (List.map (fun (M.Define (_, _, _, body)) -> get_sym_lits body) program) in
    let build_symbol sym_lit syms =
      let sym_value = L.const_string context sym_lit in
      let sym_var = L.define_global "sym_lit" sym_value the_module in
      StringMap.add sym_lit sym_var syms
    in
    StringSet.fold build_symbol sym_lits StringMap.empty
  in

  (* Association list of primitive functions names and how to build them *)
  let primitives =
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    let getchar_t = L.function_type i32_t [| |] in
    let getchar_func = L.declare_function "getchar" getchar_t the_module in

    let unit_value = L.const_int i1_t 0 in
    [
      ("print-sym", fun builder [| s |] ->
          let _ = L.build_call printf_func [| s |] "tmp" builder in
          unit_value
      );
      ("print-int", fun builder[| i |] ->
            let fmt_int = L.build_global_stringptr "%d" "fmt_int" builder in
            let _ = L.build_call printf_func [| fmt_int; i |] "tmp" builder in
            unit_value
      );
      ("print-int", fun builder[| i |] ->
            let fmt_int = L.build_global_stringptr "%d" "fmt_int" builder in
            let _ = L.build_call printf_func [| fmt_int; i |] "tmp" builder in
            unit_value
      );
      ("print-bool", fun builder[| b |] ->
            let true_str = L.build_global_stringptr "true" "true" builder in
            let false_str = L.build_global_stringptr "false" "false" builder in
            let to_print = L.build_select b true_str false_str "tmp" builder in
            let _ = L.build_call printf_func [| to_print |] "tmp" builder in
            unit_value
      );
      ("print-unit", fun builder[| b |] ->
            let unit_str = L.build_global_stringptr "unit" "unit" builder in
            let _ = L.build_call printf_func [| unit_str |] "tmp" builder in
            unit_value
      );
      ("in", fun builder[| |] -> L.build_call getchar_func[| |] "tmp" builder);

      (* Equality *)
      ("i=", fun builder [| a; b |] -> L.build_icmp L.Icmp.Eq a b "tmp" builder);
      ("s=", fun builder [| a; b |] -> L.build_icmp L.Icmp.Eq a b "tmp" builder);
      ("b=", fun builder [| a; b |] -> L.build_icmp L.Icmp.Eq a b "tmp" builder);
      ("u=", fun _ [| _; _ |] -> L.const_int i1_t 1);

      (* Arithmetic *)
      ("+", fun builder [| a; b |] -> L.build_add a b "tmp" builder);
      ("-", fun builder [| a; b |] -> L.build_sub a b "tmp" builder);
      ("*", fun builder [| a; b |] -> L.build_mul a b "tmp" builder);
      ("/", fun builder [| a; b |] -> L.build_sdiv a b "tmp" builder);
      ("%", fun builder [| a; b |] -> L.build_srem a b "tmp" builder);
      ("neg", fun builder [| a |] -> L.build_neg a "tmp" builder);
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
        let return_val = build_fun builder (L.params decl) in
        let _ = L.build_ret return_val builder in
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
  let build_function_body (M.Define (n, _, params, body)) =
    let the_function = StringMap.find n functions in

    (* Recursively build the return value of the function *)
    let rec expr locals builder =
      function
      | M.Literal l -> begin match l with
        | Ast.IntLit i -> (L.const_int i32_t i, builder)
        | Ast.SymLit s ->
          let sym = StringMap.find s symbols in
          (L.build_bitcast sym (L.pointer_type i8_t) s builder, builder)
        | Ast.BoolLit b -> if b
          then (L.const_int i1_t 1, builder)
          else (L.const_int i1_t 0, builder)
        | Ast.UnitLit -> (L.const_int i1_t 0, builder)
        end
      | M.Local n -> (StringMap.find n locals, builder)
      | M.Global n -> (StringMap.find n functions, builder)
      | M.Begin (e1, e2) ->
        let (_, builder') = expr locals builder e1 in
        expr locals builder' e2
      | M.Let (n, e, body) ->
        let (e_val, builder') = expr locals builder e in
        let locals' = StringMap.add n e_val locals in
        expr locals' builder' body
      | M.Apply (M.Global n, args) when List.mem_assoc n primitives ->
        let (arg_vals, builder') = List.fold_left
            (fun (arg_vals, b) arg ->
               let (arg_val, b') = expr locals b arg in
               (arg_vals @ [arg_val], b')
            ) ([], builder) args in
        (List.assoc n primitives builder' (Array.of_list arg_vals), builder')
      | M.Apply (f, args) ->
        let (f_val, builder') = expr locals builder f in
        let (arg_vals, builder'') = List.fold_left
            (fun (arg_vals, b) arg ->
               let (arg_val, b') = expr locals b arg in
               (arg_vals @ [arg_val], b')
            ) ([], builder') args in
        (L.build_call f_val (Array.of_list arg_vals) "apply_result" builder'', builder)
      | M.Free (n, e) ->
        let _ = L.build_free (StringMap.find n locals) builder in
        expr locals builder e
      | M.If (cond, b1, b2) ->
        let (cond_val, builder') = expr locals builder cond in

        let merge_bb = L.append_block context "merge" the_function in
        let branch_instr = L.build_br merge_bb in

        let then_bb = L.append_block context "then" the_function in
        let then_builder = L.builder_at_end context then_bb in
        let (then_val, then_builder') = expr locals then_builder b1 in
        let _ = branch_instr then_builder' in

        let else_bb = L.append_block context "else" the_function in
        let else_builder = L.builder_at_end context else_bb in
        let (else_val, else_builder') = expr locals else_builder b2 in
        let _ = branch_instr else_builder' in

        let _ = L.build_cond_br cond_val then_bb else_bb builder' in
        let merge_builder = L.builder_at_end context merge_bb in

        (L.build_phi [(then_val, L.instr_parent then_val);
                      (else_val, L.instr_parent else_val)]
           "if_result" merge_builder, merge_builder)

        (* TODO Alloc *)
        (* TODO Case *)
      | _ -> raise (Impossible "Unimplemented")
    in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let init_locals =
      let param_values = L.params the_function in
      let bindings = List.mapi (fun i n -> (n, Array.get param_values i)) params in
      List.fold_right (fun (n, v) m -> StringMap.add n v m) bindings StringMap.empty
    in
    let (body_value, builder') = expr init_locals builder body in
    let _ = L.build_ret body_value builder' in
    ()
  in
  List.iter build_function_body program;
  the_module
