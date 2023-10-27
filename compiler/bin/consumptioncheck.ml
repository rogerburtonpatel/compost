(* Consumption Checker *)
module T = Tast
module F = Fast

module S = Set.Make(String)

let unions sets = List.fold_right S.union sets S.empty

exception NameAlreadyConsumed of string

let dealloc_in to_free (expr, ty) = match to_free with
  | [] -> (expr, ty)
  | _ -> ((F.Free (to_free, (expr, ty))), ty)

let freeable bound consumed = List.filter (fun (n, _) -> S.mem n consumed) bound

let rec check_tail bound consumed (expr, ty) =
  match expr with
  | T.NameExpr (n) -> if S.mem n consumed
      then raise (NameAlreadyConsumed n)
      else (dealloc_in (freeable bound (S.remove n consumed)) (F.NameExpr n, ty), S.singleton n)
  | T.Literal (l) -> (dealloc_in (freeable bound consumed) (F.Literal l, ty), S.empty)
  | T.If (e1, e2, e3) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check_tail bound (S.union consumed c1) e2 in
    let (e3', c3) = check_tail bound (unions [consumed; c1; c2]) e3 in
    ((F.If (e1', e2', e3'), ty), unions [c1; c2; c3])
(* We only deallocate at terminal expressions in tail position *)
and check bound consumed (expr, ty) =
  match expr with
  | T.NameExpr (n) -> if S.mem n consumed
      then raise (NameAlreadyConsumed n)
      else ((F.NameExpr n, ty), S.singleton n)
  | T.Literal (l) -> ((F.Literal l, ty), S.empty)
  | T.If (e1, e2, e3) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check bound (S.union consumed c1) e2 in
    let (e3', c3) = check bound (unions [consumed; c1; c2]) e3 in
    ((F.If (e1', e2', e3'), ty), unions [c1; c2; c3])
