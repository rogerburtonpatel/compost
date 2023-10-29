module A = Ast
module U = Uast

module S = Set.Make(String)

let rec disambiguate locals = function
  | A.Literal l -> U.Literal l
  | A.NameExpr n when S.mem n locals -> U.Local n
  | A.NameExpr n -> U.Global n
  | A.Case (e, branches) ->
    let e' = disambiguate locals e in
    let branch (A.CaseBranch (p, body)) = match p with
      | A.Pattern (_, bindings) ->
        let locals' = List.fold_right (fun n ns -> S.add n ns) bindings locals in
        (U.CaseBranch (p, disambiguate locals' body))
      | A.WildcardPattern -> (U.CaseBranch (p, disambiguate locals body))
    in
    let branches' = List.map branch branches in
    U.Case (e', branches')
  | A.If (e1, e2, e3) ->
    let e1' = disambiguate locals e1 in
    let e2' = disambiguate locals e2 in
    let e3' = disambiguate locals e3 in
    U.If (e1', e2', e3')
  | A.Begin (e1, e2) ->
    let e1' = disambiguate locals e1 in
    let e2' = disambiguate locals e2 in
    U.Begin (e1', e2')
  | A.Let (n, e, body) ->
    let e' = disambiguate locals e in
    let body' = disambiguate (S.add n locals) body in
    U.Let (n, e', body')
  | A.Apply (e, es) ->
    let e' = disambiguate locals e in
    let es' = List.map (disambiguate locals) es in
    U.Apply (e', es')
  | A.Dup n -> U.Dup n
