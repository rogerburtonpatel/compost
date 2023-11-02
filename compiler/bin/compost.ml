(* Top-level of the Compost compiler *)

type action = Ast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./compost [-a|-l|-c] [file.cp]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | Compile -> ignore (Typecheck.typecheck (List.map Disambiguate.def ast))
  | _ -> ()
  (* | _ -> let sast = Semant.check ast in *)
    (* match !action with *)
    (*   Ast     -> () *)
    (* | _ -> () *)
    (* | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast)) *)
    (* | Compile -> let m = Codegen.translate sast in *)
    (*     Llvm_analysis.assert_valid_module m; *)
    (*     print_string (Llvm.string_of_llmodule m) *)
