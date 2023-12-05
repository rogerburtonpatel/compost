module P = Past
module A = Ast
module D = Difflist

module S = Set.Make(String)
module SM = Map.Make(String)

exception RecursiveUse
exception DuplicateGlobal

(* adl = Ast def list, pe = Past expr *)

let rec apes_to_ppes lb rb = function
  | [] -> []
  | ((A.Pattern(n, ns), e) :: pes) ->
    let rb = List.fold_right S.add ns rb in
    (A.Pattern(n, ns), ae_to_pe lb rb e) :: (apes_to_ppes lb rb pes)
  | ((A.WildcardPattern, e) :: pes) ->
    (A.WildcardPattern, ae_to_pe lb rb e) :: (apes_to_ppes lb rb pes)
and ae_to_pe lb rb = function
  | A.Begin([]) -> P.Literal(A.UnitLit)
  | A.Begin([e]) -> ae_to_pe lb rb e
  | A.Begin(e :: es) -> P.Let(Freshnames.fresh_name (), ae_to_pe lb rb e, ae_to_pe lb rb (A.Begin(es)))
  | A.Let([], e) -> ae_to_pe lb rb e
  | A.Let(((abn, abe) :: abs), e) ->
    let rb = S.add abn rb in
    P.Let(abn, ae_to_pe lb rb abe, ae_to_pe lb rb (A.Let(abs, e)))
  | A.Literal(l) -> P.Literal(l)
  | A.NameExpr(n) ->
    if (not (S.mem n rb)) && (SM.mem n lb)
    then SM.find n lb
    else P.NameExpr(n)
  | A.Case(e, pes) -> P.Case(ae_to_pe lb rb e, apes_to_ppes lb rb pes)
  | A.If(e1, e2, e3) -> P.If(ae_to_pe lb rb e1, ae_to_pe lb rb e2, ae_to_pe lb rb e3)
  | A.Apply(e, es) -> P.Apply(ae_to_pe lb rb e, List.map (ae_to_pe lb rb) es)
  | A.Dup(n) -> P.Dup(n)

let rec ad_to_pdl use_recur use_all let_bind top_bind = function
  | A.Use(filename) ->
    if S.mem filename use_recur then raise RecursiveUse else
    if S.mem filename use_all then (D.empty, use_all, let_bind, top_bind) else
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.program Scanner.token lexbuf in
    let use_all = S.add filename use_all in
    let use_recur = S.add filename use_recur in
    adl_to_pdl ast use_recur use_all let_bind top_bind
  | A.Val(n, e) -> 
    if S.mem n top_bind then raise DuplicateGlobal else
    let pe = ae_to_pe let_bind S.empty e in
    let let_bind = SM.add n pe let_bind in
    let top_bind = S.add n top_bind in
    (D.empty, use_all, let_bind, top_bind)
  | A.Define(n, ns, e) -> 
    if S.mem n top_bind then raise DuplicateGlobal else
    let top_bind = S.add n top_bind in
    let rb = List.fold_right S.add ns S.empty in
    let pe = ae_to_pe let_bind rb e in
    (D.singleton (P.Define(n, ns, pe)), use_all, let_bind, top_bind)
  | A.Datatype(n, ntss) -> 
    let top_bind = List.fold_left (fun top_bind (n2, _) ->
      if S.mem n2 top_bind then raise DuplicateGlobal else
      S.add n2 top_bind
    ) top_bind ntss in
    (D.singleton (P.Datatype(n, ntss)), use_all, let_bind, top_bind)
  | A.TyAnnotation(n, t) -> (D.singleton (P.TyAnnotation(n, t)), use_all, let_bind, top_bind)

and fold_adl_to_pdl use_recur pdl ad = match pdl with
    (pdl, use_all, let_bind, top_bind) -> (match (ad_to_pdl use_recur use_all let_bind top_bind ad) with
        | (pdl2, use_all2, let_bind2, top_bind2) -> (D.cons pdl pdl2, use_all2, let_bind2, top_bind2)
    )

and adl_to_pdl adl use_recur use_all let_bind top_bind = 
  List.fold_left (fold_adl_to_pdl use_recur) (D.empty, use_all, let_bind, top_bind) adl

let preprocess adeflist = match (adl_to_pdl adeflist S.empty S.empty SM.empty S.empty) with
  | (pdl, _, _, _) -> D.tolist pdl
