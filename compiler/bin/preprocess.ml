module P = Past
module A = Ast
module D = Difflist

module S = Set.Make(String)
module SM = Map.Make(String)

exception RecursiveUse
exception DuplicateTop

(* adl = Ast def list, pe = Past expr *)

let rec apes_to_ppes = function
    [] -> []
  | ((p, e) :: pes) -> (p, ae_to_pe e) :: (apes_to_ppes pes)
and ae_to_pe = function
    A.Begin([]) -> P.Literal(A.UnitLit)
  | A.Begin([e]) -> ae_to_pe e
  | A.Begin(e :: es) -> P.Let(Freshnames.fresh_name (), ae_to_pe e, ae_to_pe (A.Begin(es)))
  | A.Let([], e) -> ae_to_pe e
  | A.Let(((n, eb) :: bs), e) -> P.Let(n, ae_to_pe eb, ae_to_pe (A.Let(bs, e)))
  | A.Literal(l) -> P.Literal(l)
  | A.NameExpr(n) -> P.NameExpr(n)
  | A.Case(e, pes) -> P.Case(ae_to_pe e, apes_to_ppes pes)
  | A.If(e1, e2, e3) -> P.If(ae_to_pe e1, ae_to_pe e2, ae_to_pe e3)
  | A.Apply(e, es) -> P.Apply(ae_to_pe e, List.map ae_to_pe es)
  | A.Dup(n) -> P.Dup(n)

let rec ad_to_pdl use_all use_recur = function
    A.Use(filename) ->
    if S.mem filename use_recur then raise RecursiveUse else
    if S.mem filename use_all then (D.empty, use_all) else
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.program Scanner.token lexbuf in
    let use_all = S.add filename use_all in
    let use_recur = S.add filename use_recur in
    adl_to_pdl ast use_all use_recur
  | A.Val(n, e) -> (D.empty, use_all)
  (*| A.Val(n, e) -> (D.singleton (P.Val(n, ae_to_pe e)), use_all)*)
  | A.Define(n, ns, e) -> (D.singleton (P.Define(n, ns, ae_to_pe e)), use_all)
  | A.Datatype(n, ntss) -> (D.singleton (P.Datatype(n, ntss)), use_all)
  | A.TyAnnotation(n, t) -> (D.singleton (P.TyAnnotation(n, t)), use_all)

and fold_adl_to_pdl use_recur pdl ad = match pdl with
    (pdl, use_all) -> (match (ad_to_pdl use_all use_recur ad) with
        | (pdl2, use_all2) -> (D.cons pdl pdl2, use_all2)
    )

and adl_to_pdl adl use_all use_recur = 
  List.fold_left (fold_adl_to_pdl use_recur) (D.empty, use_all) adl

let preprocess adeflist = match (adl_to_pdl adeflist S.empty S.empty) with
  | (pdl, _) -> D.tolist pdl
