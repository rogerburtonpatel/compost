(* Consumption Checker *)
module T = Tast
module F = Fast
module N = Freshnames

module S = Set.Make(String)

let unions sets = List.fold_right S.union sets S.empty

let typeof (_, ty) = ty

exception NameAlreadyConsumed of string
exception Impossible of string

let rec check bound consumed (expr, ty) =
  match expr with
  | T.Local n when S.mem n consumed -> raise (NameAlreadyConsumed n)
  | T.Local n -> ((F.Local n, ty), S.singleton n)
  | T.Global n -> ((F.Local n, ty), S.empty)
  | T.Dup n when S.mem n consumed -> raise (NameAlreadyConsumed n)
  | T.Dup n -> ((F.Dup n, ty), S.empty)
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
  | T.Let (n, e, body) ->
    let (e', c) = check bound consumed e in
    let (body', cb) = check ((n, typeof e) :: bound) (S.union consumed c) body in
    ((F.Let (n, e', body'), ty), S.union c cb)
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
  | ((n, n_ty) :: xs) -> match n_ty with
    (* Only emit calls to _free_ functions for variant values *)
    | (Ast.CustomTy (_)) -> (F.FreeRec (n_ty, n, dealloc_in xs (expr, ty)), ty)
    | _ -> dealloc_in xs (expr, ty)

(* Note: we assume here that all names bound by nested lets are distinct *)
let freeable bound consumed = List.filter (fun (n, _) -> S.mem n consumed) bound

(* We only deallocate at the last executed terminal expression*)
let rec check_last bound consumed (expr, ty) =
  match expr with
  | T.Local n when S.mem n consumed -> raise (NameAlreadyConsumed n)
  | T.Local n -> (dealloc_in (freeable bound (S.remove n consumed)) (F.Local n, ty), S.singleton n)
  | T.Global n -> (dealloc_in (freeable bound (S.remove n consumed)) (F.Global n, ty), S.empty)
  | T.Dup n when S. mem n consumed -> raise (NameAlreadyConsumed n)
  | T.Dup n -> (dealloc_in (freeable bound (S.remove n consumed)) (F.Dup n, ty), S.empty)
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
  | T.Let (n, e, body) ->
    let (e', c) = check bound consumed e in
    let (body', cb) = check_last ((n, typeof e) :: bound) (S.union consumed c) body in
    ((F.Let (n, e', body'), ty), S.union c cb)
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
