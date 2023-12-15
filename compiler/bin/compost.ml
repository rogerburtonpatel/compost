(* Top-level of the Compost compiler *)

(* Authors: Randy Dang, Jasper Geer, Roger Burtonpatel, Jackson Warhover *)

(* Force dune to build some stuff *)
module D = Disambiguate
module T = Typecheck
module C = Consumptioncheck
module N = Normalize
module M = Memorymanage
module G = Codegen
module Pre = Preprocess

type action = Ast | PAst | UAst | TAst | MAst | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-p", Arg.Unit (set_action PAst), "Preprocess & Print the AST");
    ("-u", Arg.Unit (set_action UAst), "Print the UAST");
    ("-t", Arg.Unit (set_action TAst), "Typecheck and print UAst");
    ("-m", Arg.Unit (set_action MAst), "Typecheck, analyze consumption, add explicit memory management, and print MAst");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: dune exec -- compost [-a|-d|-t|-m|-c] [file.com]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in

  let output =
    let ast = Parser.program Scanner.token lexbuf in
    if !action = Ast then (Ast.string_of_program ast) else

    let past = Pre.preprocess ast in
    if !action = PAst then (Past.string_of_program past) else

    let uast = D.disambiguate past in
    if !action = UAst then (Uast.string_of_program uast) else

    let (type_checked, _, _) = T.typecheck uast in
    if !action = TAst then (Uast.string_of_program uast) else

    let normalized = N.normalize type_checked in

    let consumption_checked = C.consumption_check normalized in
    let memory_managed = M.mast_of_fast consumption_checked in
    if !action = MAst then (Mast.string_of_program memory_managed) else

    let m = G.codegen memory_managed in
    (* Llvm_analysis.assert_valid_module m; *)
    Llvm.string_of_llmodule m
  in print_string output
