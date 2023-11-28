module P = Past
module U = Uast
module A = Ast

module S = Set.Make(String)

let rec expr locals = function
  | P.Literal l -> U.Literal l
  | P.NameExpr n when S.mem n locals -> U.Local n
  | P.NameExpr n -> U.Global n
  | P.Case (e, branches) ->
    let e' = expr locals e in
    let branch (p, body) = match p with
      | A.Pattern (_, bindings) ->
        let locals' = S.union (S.of_list bindings) locals in
        (p, expr locals' body)
      | A.WildcardPattern -> (p, expr locals body)
    in
    let branches' = List.map branch branches in
    U.Case (e', branches')
  | P.If (e1, e2, e3) ->
    let e1' = expr locals e1 in
    let e2' = expr locals e2 in
    let e3' = expr locals e3 in
    U.If (e1', e2', e3')
  | P.Let (n, e, body) ->
    let e' = expr locals e in
    let body' = expr (S.add n locals) body in
    U.Let (n, e', body')
  | P.Apply (e, es) ->
    let e' = expr locals e in
    let es' = List.map (expr locals) es in
    U.Apply (e', es')
  | P.Dup n -> U.Dup n

let def = function
  | P.Define (n, args, body) -> U.Define (n, args, expr (S.of_list args) body)
  | P.Datatype (n, variants) -> U.Datatype (n, variants)
  | P.TyAnnotation (n, ty) -> U.TyAnnotation (n, ty)

let disambiguate = List.map def
