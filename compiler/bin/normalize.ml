module T = Tast
module N = Nast

let typeof (_, ty) = ty

let rec normalize_expr (expr, ty) = match expr with
  | T.Err n -> N.Err (ty, n)
  | T.Literal l -> N.Literal l
  | T.Local n -> N.Local n
  | T.Global n -> N.Global n
  | T.Case (e, branches) ->
    let pattern = function
      | T.Pattern (n, ns) -> N.Pattern (n, ns)
      | T.Name (n, _) -> N.Name n
    in
    let branches' =
      List.map (function (pat, e) -> (pattern pat, normalize_expr e)) branches in
    let e' = normalize_expr e in
    let n = Freshnames.fresh_name () in
    (N.Let (n, typeof e, e',
           N.Case (ty, n, branches')))
  | T.If (e1, e2, e3) ->
    let e1' = normalize_expr e1 in
    let e2' = normalize_expr e2 in
    let e3' = normalize_expr e3 in
    let n = Freshnames.fresh_name () in
    (N.Let (n, typeof e1, e1',
           N.If (n, e2', e3')))
  | T.Let (n, e, body) ->
    N.Let (n, typeof e, normalize_expr e, normalize_expr body)
  | T.Apply (e1, es) ->
    let e1' = normalize_expr e1 in
    let fun_name = Freshnames.fresh_name () in
    let names = List.init (List.length es) (fun _ -> Freshnames.fresh_name ()) in
    let binds = List.combine names es in
    let apply = N.Let (fun_name, typeof e1, e1', N.Apply (fun_name, names)) in
    List.fold_right (fun (n, e) acc -> N.Let (n, typeof e, normalize_expr e, acc)) binds apply
  | T.Dup n -> N.Dup (ty, n)

let normalize_def = function
  | T.Define (n, ty, params, body) -> N.Define (n, ty, params, normalize_expr body)
  | T.Datatype (n, variants) -> N.Datatype (n, variants)

let normalize = List.map normalize_def
