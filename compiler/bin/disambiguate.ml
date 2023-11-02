module A = Ast
module U = Uast

module S = Set.Make(String)

let rec expr locals = function
  | A.Literal l -> U.Literal l
  | A.NameExpr n when S.mem n locals -> U.Local n
  | A.NameExpr n -> U.Global n
  | A.Case (e, branches) ->
    let e' = expr locals e in
    let branch (A.CaseBranch (p, body)) = match p with
      | A.Pattern (_, bindings) ->
        let locals' = S.union (S.of_list bindings) locals in
        U.CaseBranch (p, expr locals' body)
      | A.WildcardPattern -> U.CaseBranch (p, expr locals body)
    in
    let branches' = List.map branch branches in
    U.Case (e', branches')
  | A.If (e1, e2, e3) ->
    let e1' = expr locals e1 in
    let e2' = expr locals e2 in
    let e3' = expr locals e3 in
    U.If (e1', e2', e3')
  | A.Begin (e1, e2) ->
    let e1' = expr locals e1 in
    let e2' = expr locals e2 in
    U.Begin (e1', e2')
  | A.Let (n, e, body) ->
    let e' = expr locals e in
    let body' = expr (S.add n locals) body in
    U.Let (n, e', body')
  | A.Apply (e, es) ->
    let e' = expr locals e in
    let es' = List.map (expr locals) es in
    U.Apply (e', es')
  | A.Dup n -> U.Dup n

  exception Impossible of string
let def = function
  | A.Val (n, body) -> U.Val (n, expr S.empty body)
  | A.Define (n, args, body) -> U.Define (n, args, expr (S.of_list (n :: args)) body)
  | A.Datatype (n, variants) ->
    let variant (A.Variant (n, tys)) = U.Variant (n, tys) in
    U.Datatype (n, List.map variant variants)
  | A.TyAnnotation (n, ty) -> U.TyAnnotation (n, ty)
  | A.Use _ -> raise (Impossible "use form in disambiguation")
