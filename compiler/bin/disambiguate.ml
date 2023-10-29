module A = Ast
module U = Uast

module S = Set.Make(String)

let rec disambiguate locals = function
  | A.Literal (l) -> U.Literal (l)
