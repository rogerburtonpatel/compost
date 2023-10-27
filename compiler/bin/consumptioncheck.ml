(* Consumption Checker *)
module T = Tast
module F = Fast

module S = Set.Make(String)

let unions sets = List.fold_right S.union sets S.empty

exception NameAlreadyConsumed of string

let rec consumption_check consumed_names (expr, ty) =
  match expr with
  | T.NameExpr (n) -> if S.mem n consumed_names
      then raise (NameAlreadyConsumed n)
      else ((F.NameExpr n, ty), S.singleton n)
  | T.Literal (l) -> ((F.Literal l, ty), S.empty)
  | T.If (e1, e2, e3) ->
    let (e1', c1) = consumption_check consumed_names e1 in
    let (e2', c2) = consumption_check (S.union consumed_names c1) e2 in
    let (e3', c3) = consumption_check (unions [consumed_names; c1; c2]) e3 in
    ((F.If (e1', e2', e3'), ty), unions [c1; c2; c3])
