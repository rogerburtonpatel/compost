module A = Ast
module U = Uast

module S = Set.Make(String)

let rec disambiguate locals = function
  | A.Literal l -> U.Literal l
  | A.NameExpr n when S.mem n locals -> U.Local n
  | A.NameExpr n -> U.Global n
  (* | A.Case (e, branches) -> *)
  (*   let branch (A.CaseBranch (p, body)) = match p with *)
  (*     | A.Pattern (_, bindings) -> *)
  (*       let locals' = List.fold_right (fun n ns -> S.add n ns) bindings locals in *)
  (*       (U.CaseBranch (p, disambiguate locals' body)) *)
  (*     | A.WildcardPattern -> (U.CaseBranch (p, disambiguate locals body)) *)
  (*   in *)
