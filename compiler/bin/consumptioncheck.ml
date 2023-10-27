(* Consumption Checker *)
module T = Tast
module F = Fast

module StringSet = Set.Make(String)

exception NameAlreadyConsumed of string

let rec consumption_check consumed_names (expr, ty) =
  match expr with
  | T.NameExpr (n) -> if StringSet.mem n consumed_names
      then raise (NameAlreadyConsumed n)
      else (F.NameExpr n, ty)
  | T.Literal (l) -> (F.Literal l, ty)
