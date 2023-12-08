module P = Past
module U = Uast
module A = Ast

module Prim = Primitives

module S = Set.Make(String)
module StringMap = Map.Make(String)

(* let rec freeIn n = function 
  | U.Literal _ -> false 
  | U.Global n' | U.Local n' -> n = n'
  | U.Case *)

let primty = List.fold_right (fun (n, ty) pts ->
    StringMap.add n ty pts
) Prim.primitive_tys StringMap.empty

let rec aty_to_uty = function
  | A.FunTy(tys, ty) -> U.FunTy(List.map aty_to_uty tys, aty_to_uty ty)
  | A.SingleTy(n) ->
    if StringMap.mem n primty then StringMap.find n primty else
    U.CustomTy(n)

let rec expr locals renamings = function
  | P.Literal l -> U.Literal l
  | P.NameExpr n when S.mem n locals ->
    if StringMap.mem n renamings
    then U.Local (StringMap.find n renamings)
    else U.Local n
  | P.NameExpr n -> U.Global n
  | P.Case (e, branches) ->
    let e' = expr locals renamings e in
    let branch (p, body) = match p with
      | A.Pattern (n, bindings) ->
        let rename_binding renamings name =
          if S.mem name locals
          then
            let new_name = Freshnames.fresh_name () in
            (StringMap.add name new_name renamings, new_name)
          else (renamings, name)
        in
        let (renamings', bindings') = List.fold_left_map rename_binding renamings bindings in
        let locals' = S.union (S.of_list bindings) locals in
        (U.Pattern (n, bindings'), expr locals' renamings' body)
        (* THESE NEED TO BE FIXED *)
      | A.WildcardPattern -> (U.Name ("WILDCARD", false), expr locals renamings body)
      | A.Name n -> (U.Name (n, true), expr locals renamings body)
    in
    let branches' = List.map branch branches in
    U.Case (e', branches')
  | P.If (e1, e2, e3) ->
    let e1' = expr locals renamings e1 in
    let e2' = expr locals renamings e2 in
    let e3' = expr locals renamings e3 in
    U.If (e1', e2', e3')
  | P.Let (n, e, body) when S.mem n locals ->
    let n' = Freshnames.fresh_name () in
    let e' = expr locals renamings e in
    let renamings' = StringMap.add n n' renamings in
    let body' = expr (S.add n locals) renamings' body in
    U.Let (n', e', body')
  | P.Let (n, e, body) ->
    let e' = expr locals renamings e in
    let body' = expr (S.add n locals) renamings body in
    U.Let (n, e', body')
  | P.Apply (e, es) ->
    let e' = expr locals renamings e in
    let es' = List.map (expr locals renamings) es in
    U.Apply (e', es')
  | P.Dup n -> U.Dup n

let rec vs_to_utyvs = function
  | [] -> []
  | (n, tys) :: vs -> (n, List.map aty_to_uty tys) :: vs_to_utyvs vs

let def = function
  | P.Define (n, args, body) -> U.Define (n, args, expr (S.of_list args) StringMap.empty body)
  | P.Datatype (n, variants) -> U.Datatype (n, vs_to_utyvs variants)
  | P.TyAnnotation (n, ty) -> U.TyAnnotation (n, aty_to_uty ty)

let disambiguate = List.map def
