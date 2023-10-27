(* Consumption Checker *)
module T = Tast
module F = Fast

let rec consumption_check consumed_names (expr, ty) = match expr
  T.NameExpr (n) -> true
