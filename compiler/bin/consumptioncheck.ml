(* Consumption Checker *)
module T = Tast
module F = Fast
module N = Freshnames

module S = Set.Make(String)

let unions sets = List.fold_right S.union sets S.empty

exception NameAlreadyDead of string
exception Impossible of string

let has_dataty n env = match List.assoc n env with
    | Uast.CustomTy _ -> true
    | _ -> false

let rec check live dead expr =
  match expr with
  | T.Err (ty, msg) -> (F.Err (ty, msg), dead)
  | T.Local n when S.mem n dead && has_dataty n live -> raise (NameAlreadyDead n)
  | T.Local n -> (F.Local n, S.add n dead)
  | T.Global n -> (F.Global n, dead)
  | T.Dup (_, n) when S.mem n dead -> raise (NameAlreadyDead n)
  | T.Dup (ty, n) -> (F.Dup (ty, n), dead)
  | T.Literal (l) -> (F.Literal l, dead)
  | T.If (e1, e2, e3) ->
    let (e1', c1) = check live dead e1 in
    let (e2', c2) = check live c1 e2 in
    let (e3', c3) = check live c1 e3 in
    (F.If (e1', e2', e3'), S.union c2 c3)
  | T.Let (n, e_ty, e, body) ->
    let (e', c) = check live dead e in
    let (body', cb) = check ((n, e_ty) :: live) c body in
    (F.Let (n, e', body'), cb)
  | T.Apply (e, es) ->
    let rec check_args ees c = match ees with
      | [] -> ([], c)
      | [e] ->
        let (e', c') = check live c e in
        ([e'], c')
      | (e :: es) ->
        let (e', c') = check live c e in
        let (ees', c'') = check_args es c' in
        (e' :: ees', c'')
    in
    let (e', c) = check live dead e in
    let (es', c') = check_args es c in
    (F.Apply (e', es'), c')
  | T.Case (e_ty, e, branches) ->
    let (e', c) = check live dead e in
    (* Bind the scrutinee to a name that can be freed in the branches *)
    let scrutinee_name = Freshnames.fresh_name () in
    let check_branch (pat, body) = match pat with
      | T.Pattern (n, binds) ->
        let live' = binds @ live in
        let (body', branch_c) = check live' c body in
        (* Explicity free the top level struct of the scrutinee *)
        ((F.Pattern (n, binds), F.Free (e_ty, scrutinee_name, body')), branch_c)
      | _ ->
        let (body', branch_c) = check live c body in
        ((F.WildcardPattern, F.Free (e_ty, scrutinee_name, body')), branch_c)
    in
    let (branches', branch_cs) = List.split (List.map check_branch branches) in
    (F.Let (scrutinee_name, e', F.Case (F.Local scrutinee_name, branches')), unions branch_cs)

let rec dealloc_in to_free expr =
  match to_free with
  | [] -> expr
  | ((n, n_ty) :: xs) -> match n_ty with
    (* Only emit calls to _free_ functions for variant values *)
    | (Uast.CustomTy (_)) -> F.FreeRec (n_ty, n, dealloc_in xs expr)
    | _ -> dealloc_in xs expr

(* Note: we assume here that all names live by nested lets are distinct *)
let freeable live dead = List.filter (fun (n, _) -> not (S.mem n dead)) live

(* We only deallocate at the last executed terminal expression*)
let rec check_last live dead expr =
  match expr with
  (* === Base Cases (names may be freed here) === *)
  | T.Err (ty, msg) -> (dealloc_in (freeable live dead) (F.Err (ty, msg)), dead)
  (* Fail when the programmer attempts to reference dead names *)
  | T.Local n when S.mem n dead && has_dataty n live -> raise (NameAlreadyDead n)
  (* If n is still live, consume n and free any unused live variables *)
  | T.Local n -> (dealloc_in (freeable live (S.add n dead)) (F.Local n), S.add n dead)
  (* Global names are always live *)
  | T.Global n -> (dealloc_in (freeable live dead) (F.Global n), dead)
  | T.Dup (_, n) when S.mem n dead -> raise (NameAlreadyDead n)
  | T.Dup (ty, n) -> (dealloc_in (freeable live (S.add n dead)) (F.Dup (ty, n)), dead)
  | T.Literal (l) ->
    (dealloc_in (freeable live dead) (F.Literal l), dead)
  (* === Recursive Cases === *)
  | T.If (e1, e2, e3) ->
    let (e1', c1) = check live dead e1 in
    let (e2', c2) = check_last live c1 e2 in
    let (e3', c3) = check_last live c1 e3 in
    (F.If (e1', e2', e3'), S.union c2 c3)
  | T.Let (n, e_ty, e, body) ->
    let (e', c) = check live dead e in
    let (body', cb) = check_last ((n, e_ty) :: live) c body in
    (F.Let (n, e', body'), cb)
  | T.Apply (e, []) ->
    let (e', c) = check_last live dead e in
    (F.Apply (e', []), c)
  | T.Apply (e, es) ->
    let rec check_args ees c = match ees with
      | [] -> raise (Impossible "This case should handle only Apply with non-empty arguments")
      | [e] ->
        let (e', c') = check_last live c e in
        ([e'], c')
      | (e :: es) ->
        let (e', c') = check live c e in
        let (ees', c'') = check_args es c' in
        (e' :: ees', c'')
    in
    let (e', c) = check live dead e in
    let (es', c') = check_args es c in
    (F.Apply (e', es'), c')
  | T.Case (e_ty, e, branches) ->
    let (e', c) = check live dead e in
    (* Bind the scrutinee to a name that can be freed in the branches *)
    let scrutinee_name = Freshnames.fresh_name () in
    let check_branch (pat, body) = match pat with
      | T.Pattern (n, binds) ->
        let live' = binds @ live in
        let (body', branch_c) = check_last live' c body in
        (* Explicity free the top level struct of the scrutinee *)
        ((F.Pattern (n, binds), F.Free (e_ty, scrutinee_name, body')), branch_c)
      | _ ->
        let (body', branch_c) = check_last live c body in
        ((F.WildcardPattern, F.Free (e_ty, scrutinee_name, body')), branch_c)
    in
    let (branches', branch_cs) = List.split (List.map check_branch branches) in
    (F.Let (scrutinee_name, e', F.Case (F.Local scrutinee_name, branches')), unions (c :: branch_cs))

let check_def = function
  | T.Define (fun_name, Uast.FunTy (param_tys, return_ty), params, body) ->
    let fun_ty = Uast.FunTy (param_tys, return_ty) in
    let init_live = List.combine params param_tys in
    let (body', _) = check_last init_live S.empty body in
    F.Define (fun_name, fun_ty, params, body')
  | T.Define _ -> raise (Impossible "function with non-function type")
  | T.Datatype (name, variants) ->
    F.Datatype (name, variants)

let consumption_check = List.map check_def
