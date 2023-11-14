(* Top-level of the Compost compiler *)

(* Force dune to build some stuff (REMOVE LATER) *)
module D = Disambiguate
module T = Typecheck
module C = Consumptioncheck
module M = Memorymanage
module G = Codegen
module Pre = Preprocess

type action = Ast | Pre | UAst | TAst | MAst | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-p", Arg.Unit (set_action Pre), "Preprocess & Print the AST");
    ("-d", Arg.Unit (set_action UAst), "Print the UAST");
    ("-t", Arg.Unit (set_action TAst), "Typecheck and print UAst");
    ("-m", Arg.Unit (set_action MAst), "Typecheck, analyze consumption, add explicit memory management, and print MAst");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: dune exec -- compost [-a|-d|-t|-m|-c] [file.com]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast ->
    print_string (Ast.string_of_program ast)
  | Pre ->
    let pre = Pre.preprocess ast in
    print_string (Ast.string_of_program pre)
  | UAst ->
    let pre = Pre.preprocess ast in
    let uast = D.disambiguate pre in
    print_string(Uast.string_of_program uast)
  | TAst ->
    let pre = Pre.preprocess ast in
    let uast = D.disambiguate pre in
    let _ = Typecheck.typecheck uast in
    print_string (Uast.string_of_program uast)
  | MAst ->
    let pre = Pre.preprocess ast in
    let disambiguated = D.disambiguate pre in
    let (type_checked, _, _) = T.typecheck disambiguated in
    let consumption_checked = C.consumption_check type_checked in
    let memory_managed = M.mast_of_fast consumption_checked in 
    print_string (Mast.string_of_program memory_managed)
  | Compile ->
    let pre = Pre.preprocess ast in
    let disambiguated = D.disambiguate pre in
    let (type_checked, _, _) = T.typecheck disambiguated in
    let consumption_checked = C.consumption_check type_checked in
    let memory_managed = M.mast_of_fast consumption_checked in
    let m = G.codegen memory_managed in
    (* Llvm_analysis.assert_valid_module m; *)
    print_string (Llvm.string_of_llmodule m)
