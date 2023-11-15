module P = Past
module A = Ast
module D = Difflist

exception UseDepth
let maxdepth = 50

(* adl = Ast def list, pe = Past expr *)

let rec apes_to_ppes = function
    [] -> []
  | ((p, e) :: pes) -> (p, ae_to_pe e) :: (apes_to_ppes pes)
and ae_to_pe = function
    A.Begin([]) -> P.Literal(A.UnitLit)
  | A.Begin([e]) -> ae_to_pe e
  | A.Begin(e :: es) -> P.Begin(ae_to_pe e, ae_to_pe (A.Begin(es)))
  | A.Let([], e) -> ae_to_pe e
  | A.Let(((n, eb) :: bs), e) -> P.Let(n, ae_to_pe eb, ae_to_pe (A.Let(bs, e)))
  | A.Literal(l) -> P.Literal(l)
  | A.NameExpr(n) -> P.NameExpr(n)
  | A.Case(e, pes) -> P.Case(ae_to_pe e, apes_to_ppes pes)
  | A.If(e1, e2, e3) -> P.If(ae_to_pe e1, ae_to_pe e2, ae_to_pe e3)
  | A.Apply(e, es) -> P.Apply(ae_to_pe e, List.map ae_to_pe es)
  | A.Dup(n) -> P.Dup(n)

let rec ad_to_pdl depth = function
    A.Use(filename) ->
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.program Scanner.token lexbuf in
    adl_to_pdl ast (depth + 1)
  | A.Val(n, e) -> D.singleton (P.Val(n, ae_to_pe e))
  | A.Define(n, ns, e) -> D.singleton (P.Define(n, ns, ae_to_pe e))
  | A.Datatype(n, ntss) -> D.singleton (P.Datatype(n, ntss))
  | A.TyAnnotation(n, t) -> D.singleton (P.TyAnnotation(n, t))

and fold_adl_to_pdl depth ad pdl = D.cons (ad_to_pdl depth ad) pdl

and adl_to_pdl adl depth = 
  if depth > maxdepth then raise UseDepth else
  List.fold_right (fold_adl_to_pdl depth) adl (D.empty)

let preprocess adeflist = D.tolist (adl_to_pdl adeflist 0)
