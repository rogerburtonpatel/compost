module L = Llvm
module M = Mast
module P = Primitives

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let ctx = L.create_context

exception Impossible of string

let codegen program variant_idx_map =
  let context = L.global_context () in
  let i64_t = L.i64_type context
  and i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and void_t = L.void_type context
  and the_module = L.create_module context "Compost" in

  let rec lltype_of_ty = function
    | M.Int i -> L.integer_type context i
    | M.Fun (ret_ty, param_tys) ->
      let ret_ty' = lltype_of_ty ret_ty in
      let param_tys' = List.map lltype_of_ty param_tys in
      L.function_type ret_ty' (Array.of_list param_tys')
    | M.Struct tys ->
      let tys' = List.map lltype_of_ty tys in
      L.struct_type context (Array.of_list tys')
    | M.Ptr ty -> L.pointer_type (lltype_of_ty ty)
  in
  let unions sets = List.fold_right StringSet.union sets StringSet.empty in

  let symbols =
    let rec get_sym_lits = function
      | M.Literal (Ast.SymLit str) -> StringSet.singleton str
      | M.Case (e, branches) ->
        let branch_lits = unions (List.map (fun (_, e) -> get_sym_lits e) branches) in
        StringSet.union (get_sym_lits e) branch_lits
      | M.If (e1, e2, e3) -> unions [get_sym_lits e1; get_sym_lits e2; get_sym_lits e3]
      | M.Let (_, e1, e2) -> unions [get_sym_lits e1; get_sym_lits e2]
      | M.Apply (e, args) -> unions ((get_sym_lits e) :: (List.map get_sym_lits args))
      | M.Free (_, e) -> get_sym_lits e
      | _ -> StringSet.empty
    in
    let sym_lits = unions (List.map (fun (M.Define (_, _, _, body)) -> get_sym_lits body) program) in
    let build_symbol sym_lit syms =
      let sym_value = L.const_stringz context sym_lit in
      let sym_var = L.define_global "sym_lit" sym_value the_module in
      StringMap.add sym_lit sym_var syms
    in
    StringSet.fold build_symbol sym_lits StringMap.empty
  in

  let abort_t = L.function_type void_t [| |] in
  let abort_func = L.declare_function "abort" abort_t the_module in

  (* Association list of primitive functions names and how to build them *)
  let primitives =
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    let getchar_t = L.function_type i32_t [| |] in
    let getchar_func = L.declare_function "getchar" getchar_t the_module in

    let unit_value = L.const_int i1_t 0 in
    [
      ("print-newline", fun builder ->
          function
          | [| |] ->
            let newline = L.build_global_stringptr "\n" "newline" builder in
            let _ = L.build_call printf_func [| newline |] "tmp" builder in
            unit_value
          | _ -> raise (Impossible "print-sym has 1 parameter")

      );
      ("print-sym", fun builder ->
          function
          | [| s |] ->
            let _ = L.build_call printf_func [| s |] "tmp" builder in
            unit_value
          | _ -> raise (Impossible "print-sym has 1 parameter")
      );
      ("print-int", fun builder ->
         function
         | [| i |] ->
            let fmt_int = L.build_global_stringptr "%d" "fmt_int" builder in
            let _ = L.build_call printf_func [| fmt_int; i |] "tmp" builder in
            unit_value
         | _ -> raise (Impossible "print-int has 1 parameter")
      );
      ("print-bool", fun builder ->
          function
          | [| b |] ->
            let true_str = L.build_global_stringptr "true" "true" builder in
            let false_str = L.build_global_stringptr "false" "false" builder in
            let to_print = L.build_select b true_str false_str "tmp" builder in
            let _ = L.build_call printf_func [| to_print |] "tmp" builder in
            unit_value
          | _ -> raise (Impossible "print-bool has 1 parameter")
      );
      ("print-unit", fun builder ->
          function
          | [| _ |] ->
            let unit_str = L.build_global_stringptr "unit" "unit" builder in
            let _ = L.build_call printf_func [| unit_str |] "tmp" builder in
            unit_value
          | _ -> raise (Impossible "print-unit has 1 parameter")
      );
      ("in", fun builder ->
          function
          | [| |] -> L.build_call getchar_func [| |] "tmp" builder
          | _ -> raise (Impossible "in has no parameters")
      );

      (* Equality *)
      ("i=", fun builder ->
          function
          | [| a; b |] -> L.build_icmp L.Icmp.Eq a b "tmp" builder
          | _ -> raise (Impossible "i= has 2 parameters")
      );
      ("s=", fun builder ->
          function
          | [| a; b |] -> L.build_icmp L.Icmp.Eq a b "tmp" builder
          | _ -> raise (Impossible "s= has 2 parameters")
      );
      ("b=", fun builder ->
          function
          | [| a; b |] -> L.build_icmp L.Icmp.Eq a b "tmp" builder
          | _ -> raise (Impossible "b= has 2 parameters")
      );
      ("u=", fun _ ->
          function
          | [| _; _ |] -> L.const_int i1_t 1
          | _ -> raise (Impossible "u= has 2 parameters")
      );

      (* Arithmetic *)
      ("+", fun builder ->
          function
          | [| a; b |] -> L.build_add a b "tmp" builder
          | _ -> raise (Impossible "+ has 2 parameters")
      );
      ("-", fun builder ->
          function
          | [| a; b |] -> L.build_sub a b "tmp" builder
          | _ -> raise (Impossible "- has 2 parameters")
      );
      ("*", fun builder ->
          function
          | [| a; b |] -> L.build_mul a b "tmp" builder
          | _ -> raise (Impossible "* has 2 parameters")
      );
      ("/", fun builder ->
          function
          | [| a; b |] -> L.build_sdiv a b "tmp" builder
          | _ -> raise (Impossible "/ has 2 parameters")
      );
      ("%", fun builder ->
          function
          | [| a; b |] -> L.build_srem a b "tmp" builder
          | _ -> raise (Impossible "% has 2 parameters")
      );
      ("neg", fun builder ->
          function
          | [| a |] -> L.build_neg a "tmp" builder
          | _ -> raise (Impossible "neg has 2 parameters")
      );

      (* Comparison *)
      (">", fun builder ->
          function
          | [| a; b |] -> L.build_icmp L.Icmp.Sgt a b "tmp" builder
          | _ -> raise (Impossible "> has 2 parameters")
      );
      ("<", fun builder ->
          function
          | [| a; b |] -> L.build_icmp L.Icmp.Slt a b "tmp" builder
          | _ -> raise (Impossible "< has 2 parameters")
      );
      (">=", fun builder ->
          function
          | [| a; b |] -> L.build_icmp L.Icmp.Sge a b "tmp" builder
          | _ -> raise (Impossible ">= has 2 parameters")
      );
      ("<=", fun builder ->
          function
          | [| a; b |] -> L.build_icmp L.Icmp.Sle a b "tmp" builder
          | _ -> raise (Impossible "<= has 2 parameters")
      );
    ]
  in

  (* Build the set of function declarations *)
  let functions =
    let primitive_decls =
      let build_primitive (fun_name, build_fun) decls =
        let fun_ty = List.assoc fun_name P.primitives in
        let fun_lltype = lltype_of_ty (Memorymanage.convert_builtin_ty fun_ty) in
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
    let rec expr is_tail locals builder =
      let non_tail = expr false in
      let tail = expr is_tail in
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
      | M.Let (n, e, body) ->
        let (e_val, builder') = non_tail locals builder e in
        let locals' = StringMap.add n e_val locals in
        tail locals' builder' body
      | M.Apply (M.Global n, args) when List.mem_assoc n primitives ->
        let (arg_vals, builder') = List.fold_left
            (fun (arg_vals, b) arg ->
               let (arg_val, b') = non_tail locals b arg in
               (arg_vals @ [arg_val], b')
            ) ([], builder) args in
        (List.assoc n primitives builder' (Array.of_list arg_vals), builder')
      | M.Apply (f, args) ->
        let (f_val, builder') = non_tail locals builder f in
        let (arg_vals, builder'') = List.fold_left
            (fun (arg_vals, b) arg ->
               let (arg_val, b') = non_tail locals b arg in
               (arg_vals @ [arg_val], b')
            ) ([], builder') args in
        let call_val = L.build_call f_val (Array.of_list arg_vals) "apply_result" builder'' in
        L.set_tail_call is_tail call_val;
        (call_val, builder)
      | M.Free (n, e) ->
        let _ = L.build_free (StringMap.find n locals) builder in
        tail locals builder e
      | M.If (cond, b1, b2) when is_tail ->
        let (cond_val, builder') = non_tail locals builder cond in

        let then_bb = L.append_block context "then" the_function in
        let then_builder = L.builder_at_end context then_bb in
        let (then_val, then_builder') = tail locals then_builder b1 in
        let _ = L.build_ret then_val then_builder' in

        let else_bb = L.append_block context "else" the_function in
        let else_builder = L.builder_at_end context else_bb in
        let (else_val, else_builder') = tail locals else_builder b2 in
        let _ = L.build_ret else_val else_builder' in

        let _ = L.build_cond_br cond_val then_bb else_bb builder' in

        let branch_ty = L.type_of else_val in
        let bogus_val = match L.classify_type branch_ty with
          | L.TypeKind.Pointer ->
            let bogus_int = L.const_int i64_t 0 in
            L.build_inttoptr bogus_int branch_ty "tmp" builder'
          | L.TypeKind.Integer ->
            let bitwidth = L.integer_bitwidth branch_ty in
            L.const_int (L.integer_type context bitwidth) 0
          | _ -> raise (Impossible "Non-pointer, non-integer return type")
        in

        (* Throw up something for the enclosing call to use - we will never return this *)
        (bogus_val, builder')

      | M.If (cond, b1, b2) ->
        let (cond_val, builder') = non_tail locals builder cond in

        let merge_bb = L.append_block context "merge" the_function in
        let branch_instr = L.build_br merge_bb in

        let then_bb = L.append_block context "then" the_function in
        let then_builder = L.builder_at_end context then_bb in
        let (then_val, then_builder') = non_tail locals then_builder b1 in
        let then_val' = then_val in
        let _ = branch_instr then_builder' in

        let else_bb = L.append_block context "else" the_function in
        let else_builder = L.builder_at_end context else_bb in
        let (else_val, else_builder') = non_tail locals else_builder b2 in
        let else_val' = else_val in
        let _ = branch_instr else_builder' in

        let _ = L.build_cond_br cond_val then_bb else_bb builder' in
        let merge_builder = L.builder_at_end context merge_bb in

        (L.build_phi [(then_val', L.insertion_block then_builder');
                      (else_val', L.insertion_block else_builder')]
           "if_result" merge_builder, merge_builder)

      | M.Alloc (ty, tag, args) ->
        let struct_val = L.build_malloc (lltype_of_ty ty) "struct" builder in
        let tag_val = L.const_int i32_t tag in
        let tag_ptr = L.build_struct_gep struct_val 0 "tag_ptr" builder in
        let _ = L.build_store tag_val tag_ptr builder in
        let (builder', _) = List.fold_left
            (fun (b, i) arg ->
              let (arg_val, b') = non_tail locals b arg in
              let convert_to_i64 v = match L.classify_type (L.type_of v) with
                | L.TypeKind.Pointer -> L.build_ptrtoint v i64_t "tmp" b'
                | _ -> L.build_zext v i64_t "tmp" b'
              in
              let converted_val = convert_to_i64 arg_val in
              let elem_ptr = L.build_struct_gep struct_val i "elem_ptr" b' in
              let _ = L.build_store converted_val elem_ptr b' in
              (b', i + 1)
            ) (builder, 1) args in
        (struct_val, builder')
      | M.Err msg ->
        let msg_str = L.build_global_stringptr msg "err_msg" builder in
        let _ = List.assoc "print-sym" primitives builder [| msg_str |]in
        (L.build_call abort_func [| |] "tmp" builder, builder)

      | M.Case (scrutinee, branches) ->
        let (scrutinee_val, builder') = non_tail locals builder scrutinee in
        let tag_ptr = L.build_struct_gep scrutinee_val 0 "tag_ptr" builder' in (* error here *)
        let tag_val = L.build_load tag_ptr "tag_val" builder' in
        let default_bb = L.append_block context "default" the_function in
        let switch = L.build_switch tag_val default_bb (List.length branches) builder' in

        let merge_bb = L.append_block context "merge" the_function in
        let branch_instr = L.build_br merge_bb in

        let build_branch (pat, body) = match pat with
            | M.Pattern(variant_name, names) ->
              let branch_bb = L.append_block context "case_branch" the_function in
              let branch_builder = L.builder_at_end context branch_bb in
              let convert_i64 ty v = match ty with
                | M.Int n ->
                  let in_ty = L.integer_type context n in
                  L.build_trunc v in_ty "tmp" branch_builder
                | _ -> L.build_inttoptr v (lltype_of_ty ty) "tmp" branch_builder
              in
              let (locals', _) = List.fold_left
                                  (fun (locals, i) (n, ty) ->
                                    let arg_ptr = L.build_struct_gep scrutinee_val i "arg_ptr" branch_builder in
                                    let arg_val = L.build_load arg_ptr "arg_val" branch_builder in
                                    let converted_val = convert_i64 ty arg_val in
                                    (StringMap.add n converted_val locals, i + 1)
                                  ) (locals, 1) names
              in
              let (body_val, body_builder (* ha ha *)) = non_tail locals' branch_builder body in
              let idx_val = L.const_int i32_t (StringMap.find variant_name variant_idx_map) in
              let _ = L.add_case switch idx_val branch_bb in
              let _ = branch_instr body_builder in
              (body_val, L.insertion_block body_builder)
            | M.WildcardPattern ->
              let branch_builder = L.builder_at_end context default_bb in
              let (body_val, body_builder) = non_tail locals branch_builder body in
              let _ = branch_instr body_builder in
              (body_val, L.insertion_block body_builder)
        in
        let merge_builder = L.builder_at_end context merge_bb in
        (L.build_phi (List.map build_branch branches) "case_result" merge_builder, merge_builder)
    in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let init_locals =
      let param_values = L.params the_function in
      let bindings = List.mapi (fun i n -> (n, Array.get param_values i)) params in
      List.fold_right (fun (n, v) m -> StringMap.add n v m) bindings StringMap.empty
    in
    let (body_value, builder') = expr true init_locals builder body in
    let _ = L.build_ret body_value builder' in
    ()
  in
  List.iter build_function_body program;
  the_module
