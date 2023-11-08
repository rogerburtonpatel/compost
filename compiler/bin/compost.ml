(* Top-level of the Compost compiler *)

(* Force dune to build some stuff (REMOVE LATER) *)
module D = Disambiguate
module T = Typecheck
module C = Consumptioncheck
module M = Memorymanage
module G = Codegen

type action = Ast | UAst | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-d", Arg.Unit (set_action UAst), "Print the UAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./compost [-a|-l|-c] [file.com]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | UAst -> print_string (Uast.string_of_program (D.disambiguate ast))
  | Compile ->
    let disambiguated = D.disambiguate ast in
    let (type_checked, _, _) = T.typecheck disambiguated in
    let consumption_checked = C.consumption_check type_checked in
    let memory_managed = M.mast_of_fast consumption_checked in
    let m = G.codegen memory_managed in
    (* Llvm_analysis.assert_valid_module m; *)
    print_string (Llvm.string_of_llmodule m)
  | _ -> ()
