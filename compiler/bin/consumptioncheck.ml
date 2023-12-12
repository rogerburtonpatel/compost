(* Consumption Checker *)
module N = Nast
module F = Fast
module T = Tast

module S = Set.Make(String)
module StringMap = Map.Make(String)

let unions sets = List.fold_right S.union sets S.empty

exception NameNotLive of string
exception Impossible of string

let rec not_free =
  function
  | N.Err _ -> S.empty
  | N.Local n -> S.singleton n
  | N.Global _ -> S.empty
  | N.Dup _ -> S.empty
  | N.Literal _ -> S.empty
  | N.If (n, e1, e2) -> S.add n (unions [not_free e1; not_free e2])
  | N.Let (_, _, e, body) -> unions [not_free e; not_free body]
  | N.Apply (n, ns) -> S.of_list (n :: ns)
  | N.Case (_, n, branches) -> S.add n (unions (List.map (function (_, branch) -> not_free branch) branches))

let is_dataty = function
  | Uast.CustomTy _ -> true
  | _ -> false

let merge = StringMap.union
    (fun n ty1 ty2 ->
       if ty1 = ty2
       then Some ty1
       else raise (Impossible ("type of " ^ n ^ " is not consistent.")))

let consume_in to_consume expr =
  StringMap.fold (fun n ty acc ->
      if is_dataty ty
      then F.FreeRec (ty, n, acc)
      else acc) to_consume expr

let rec insert_frees to_consume =
  function
  | N.If (n, e1, e2) ->
    let to_consume' = StringMap.remove n to_consume in
    let e1' = insert_frees to_consume' e1 in
    let e2' = insert_frees to_consume' e2 in
    F.If (F.Local n, e1', e2')
  | N.Case (ty, scrutinee, branches) ->
    let to_consume' = StringMap.remove scrutinee to_consume in
    let branch (pattern, body) = match pattern with
      | N.Pattern (tag, binds) ->
        let introduced =
          List.fold_right
            (fun (n, ty) acc -> StringMap.add n ty acc) binds StringMap.empty
        in
        let body' = insert_frees (merge introduced to_consume') body in
        (F.Pattern (tag, binds), F.Free (ty, scrutinee, body'))
      | N.Name n ->
        let body' = insert_frees (StringMap.add n ty to_consume) body in
        (F.Name n, F.Let (n, F.Local scrutinee, body'))
    in
    F.Case (F.Local scrutinee, List.map branch branches)
  | N.Let (n, ty, e, body) ->
    let consumed_in_e = not_free e in
    let to_consume_e = StringMap.filter (fun n _ -> S.mem n consumed_in_e) to_consume in
    let to_consume_body = StringMap.add n ty
        (StringMap.filter (fun n _ -> not (S.mem n consumed_in_e)) to_consume)
    in
    let e' = insert_frees to_consume_e e in
    let body' = insert_frees to_consume_body body in
    F.Let (n, e', body')
  | N.Apply (n, ns) ->
    let to_consume' = List.fold_right (fun n acc -> StringMap.remove n acc) (n :: ns) to_consume in
    consume_in to_consume' (F.Apply (F.Local n, List.map (fun n -> F.Local n) ns))
  | N.Dup (ty, n) ->
    let to_consume' = StringMap.remove n to_consume in
    consume_in to_consume' (F.Dup (ty, n))
  | N.Literal l -> consume_in to_consume (F.Literal l)
  | N.Global n -> consume_in to_consume (F.Global n)
  | N.Local n ->
    let to_consume' = StringMap.remove n to_consume in
    consume_in to_consume' (F.Local n)
  | N.Err (ty, msg) -> consume_in to_consume (F.Err (ty, msg))

let insert_frees_def =
  function
  | N.Define (fun_name, fun_ty, params, body) ->
    let param_tys = match fun_ty with
                    | Uast.FunTy (param_tys, _) -> param_tys
                    | _ -> raise (Impossible "function does not have function type")
    in
    let typed_params = List.combine params param_tys in
    let params_to_consume = List.fold_right
        (fun (n, ty) acc -> StringMap.add n ty acc) typed_params StringMap.empty in
    let body' = insert_frees params_to_consume body in
    F.Define(fun_name, fun_ty, params, body')
  | N.Datatype (n, variants) -> F.Datatype (n, variants)

let consumption_check = List.map insert_frees_def
