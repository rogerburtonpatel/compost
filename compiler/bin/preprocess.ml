exception UseDepth

let maxdepth = 50

let rec preprocessdepth deflist depth = List.fold_right (fun def deflist -> 
  if depth > maxdepth then raise UseDepth else
  match (def, deflist) with
   | (Ast.Use(filename), deflist) ->
     let channel = open_in filename in
     let lexbuf = Lexing.from_channel channel in
     let ast = Parser.program Scanner.token lexbuf in
     preprocessdepth (List.append ast deflist) (depth + 1)
   | (otherdef, deflist) ->
     otherdef::deflist
) deflist []

let preprocess deflist = preprocessdepth deflist 0

(*
let rec desugar_let bindings body = match bindings with
  | [] -> body
  | ((name, expr) :: bs) -> Let(name, expr, desugar_let bs body)

let rec desugar_begin exprs = match exprs with
  | [] -> Literal(UnitLit)
  | [e] -> e
  | (e :: es) -> Begin(e, desugar_begin es)
*)
