(* Consumption Checker *)
module T = Tast
module F = Fast

module S = Set.Make(String)

let unions sets = List.fold_right S.union sets S.empty

let typeof (_, ty) = ty

exception NameAlreadyConsumed of string
exception Impossible of string

let rec check bound consumed (expr, ty) =
  match expr with
  | T.NameExpr (n) -> if S.mem n consumed
      then raise (NameAlreadyConsumed n)
      else ((F.NameExpr n, ty), S.singleton n)
  | T.Dup (n) -> if S.mem n consumed
      then raise (NameAlreadyConsumed n)
      else ((F.Dup n, ty), S.empty)
  | T.Literal (l) -> ((F.Literal l, ty), S.empty)
  | T.If (e1, e2, e3) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check bound (S.union consumed c1) e2 in
    let (e3', c3) = check bound (unions [consumed; c1; c2]) e3 in
    ((F.If (e1', e2', e3'), ty), unions [c1; c2; c3])
  | T.Begin (e1, e2) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check bound (S.union consumed c1) e2 in
    ((F.Begin (e1', e2'), ty), S.union c1 c2)
  | T.Let (n, e1, e2) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check ((n, typeof e1) :: bound) (S.union consumed c1) e2 in
    ((F.Let (n, e1', e2'), ty), S.union c1 c2)
  | T.Apply (e, es) ->
    let rec check_args ees c = match ees with
      | [] -> ([], c)
      | [e] ->
        let (e', c') = check bound c e in
        ([e'], c')
      | (e :: es) ->
        let (e', c') = check bound c e in
        let (ees', c'') = check_args es (S.union c c') in
        (e' :: ees', c'')
    in
    let (e', c) = check bound consumed e in
    let (es', c') = check_args es c in
    ((F.Apply (e', es'), ty), c')

let rec dealloc_in to_free (expr, ty) = match to_free with
  | [] -> (expr, ty)
  | ((n, n_ty) :: xs) -> (F.Free (n_ty, n, dealloc_in xs (expr, ty)), ty)

(* Note: we assume here that all names bound by nested lets are distinct *)
let freeable bound consumed = List.filter (fun (n, _) -> S.mem n consumed) bound

(* We only deallocate at the last executed terminal expression*)
let rec check_last bound consumed (expr, ty) =
  match expr with
  | T.NameExpr (n) -> if S.mem n consumed
      then raise (NameAlreadyConsumed n)
      else (dealloc_in (freeable bound (S.remove n consumed)) (F.NameExpr n, ty), S.singleton n)
  | T.Dup (n) -> if S.mem n consumed
      then raise (NameAlreadyConsumed n)
      else (dealloc_in (freeable bound (S.remove n consumed)) (F.Dup n, ty), S.empty)
  | T.Literal (l) -> (dealloc_in (freeable bound consumed) (F.Literal l, ty), S.empty)
  | T.If (e1, e2, e3) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check_last bound (S.union consumed c1) e2 in
    let (e3', c3) = check_last bound (unions [consumed; c1; c2]) e3 in
    ((F.If (e1', e2', e3'), ty), unions [c1; c2; c3])
  | T.Begin (e1, e2) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check_last bound (S.union consumed c1) e2 in
    ((F.Begin (e1', e2'), ty), S.union c1 c2)
  | T.Let (n, e1, e2) ->
    let (e1', c1) = check bound consumed e1 in
    let (e2', c2) = check_last ((n, typeof e1) :: bound) (S.union consumed c1) e2 in
    ((F.Let (n, e1', e2'), ty), S.union c1 c2)
  | T.Apply (e, []) ->
    let (e', c) = check_last bound consumed e in
    ((F.Apply (e', []), ty), c)
  | T.Apply (e, es) ->
    let rec check_args ees c = match ees with
      | [] -> raise (Impossible "This case should handle only Apply with non-empty arguments")
      | [e] ->
        let (e', c') = check_last bound c e in
        ([e'], c')
      | (e :: es) ->
        let (e', c') = check bound c e in
        let (ees', c'') = check_args es (S.union c c') in
        (e' :: ees', c'')
    in
    let (e', c) = check bound consumed e in
    let (es', c') = check_args es c in
    ((F.Apply (e', es'), ty), c')
