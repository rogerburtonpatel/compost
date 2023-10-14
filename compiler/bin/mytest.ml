(* Top level file that drives scanner and parser *)
open Toplevel
let () =
    let usage_msg = "usage: ./compost.native [file.compost]" in
    let channel = ref stdin in
    Arg.parse [] (fun file -> channel := open_in file) usage_msg;
    
    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.token lexbuf in

    (* let lexbuf = Lexing.from_string "(val x 3)" in
    let ast = Parser.program Scanner.token lexbuf in *)
    (* begin_testing_zone *)

    (* let rec check_ast a = 
        let rec check_exp exp = match exp with 
        Ast.Val (s, _) -> print_endline ("the name is " ^ s)
        | _ -> raise (Failure("nah\n"))
    in 
    match a with 
    [] -> print_endline "end of program"
    | (ast_head :: ast_rest) -> check_exp ast_head ; check_ast ast_rest
    in check_ast ast *)
(* ^ generic traversal, edit as needed *)

    (* end_testing_zone *)

    print_string (Ast.string_of_program ast) ;
    (* let%test _ = Ast.string_of_program = "(val h 'hello world')" *)
    (* let%test _ = 5 = 6 *)

