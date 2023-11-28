module P = Past
module A = Ast
module D = Difflist

module S = Set.Make(String)
module SM = Map.Make(String)

exception RecursiveUse
exception DuplicateDef

(* adl = Ast def list, pe = Past expr *)

let rec apes_to_ppes = function
    [] -> []
  | ((p, e) :: pes) -> (p, ae_to_pe e) :: (apes_to_ppes pes)
and ae_to_pe b = function
    A.Begin([]) -> P.Literal(A.UnitLit)
  | A.Begin([e]) -> ae_to_pe b e
  | A.Begin(e :: es) -> P.Let(Freshnames.fresh_name (), ae_to_pe b e, ae_to_pe b (A.Begin(es)))
  | A.Let([], e) -> ae_to_pe b e
  | A.Let(((n, eb) :: lbs), e) -> P.Let(n, ae_to_pe b eb, ae_to_pe (S.add n b) (A.Let(lbs, e)))
  | A.Literal(l) -> P.Literal(l)
  | A.NameExpr(n) -> P.NameExpr(n)
  | A.Case(e, pes) -> P.Case(ae_to_pe b e, apes_to_ppes pes)
  | A.If(e1, e2, e3) -> P.If(ae_to_pe b e1, ae_to_pe e2, ae_to_pe e3)
  | A.Apply(e, es) -> P.Apply(ae_to_pe b e, List.map (ae_to_pe b) es)
  | A.Dup(n) -> P.Dup(n)

let rec ad_to_pdl use_recur trackers = function
    A.Use(filename) ->
    if S.mem filename use_recur then raise RecursiveUse else
    match trackers with (
      | (use_all, let_bind, top_bind) -> 
        if S.mem filename use_all then (D.empty, use_all) else
        let channel = open_in filename in
        let lexbuf = Lexing.from_channel channel in
        let ast = Parser.program Scanner.token lexbuf in
        let use_all = S.add filename use_all in
        let use_recur = S.add filename use_recur in
        adl_to_pdl ast use_all use_recur let_bind
    )
  | A.Val(n, e) -> 
    if SM.mem n let_bind then raise DuplicateDef else
    (D.empty, use_all, SM.add n (ae_to_pe e S.empty) let_bind)
  | A.Define(n, ns, e) -> 
    if SM.mem n let_bind then raise DuplicateDef else
    (D.singleton (P.Define(n, ns, ae_to_pe e (S.singleton n))), use_all, let_bind)
  | A.Datatype(n, ntss) -> (D.singleton (P.Datatype(n, ntss)), use_all, let_bind)
  | A.TyAnnotation(n, t) -> (D.singleton (P.TyAnnotation(n, t)), use_all, let_bind)
    )

and fold_adl_to_pdl use_recur pdl ad = match pdl with
    (pdl, trackers) -> (match (ad_to_pdl use_recur trackers ad) with
        | (pdl2, trackers2) -> (D.cons pdl pdl2, trackers2)
    )

and adl_to_pdl adl use_recur trackers = 
  List.fold_left (fold_adl_to_pdl use_recur) (D.empty, trackers) adl

let preprocess adeflist = 
  let trackers = (S.empty, SM.empty, S.empty) in
  match (adl_to_pdl adeflist S.empty trackers) with
    | (pdl, _) -> D.tolist pdl

