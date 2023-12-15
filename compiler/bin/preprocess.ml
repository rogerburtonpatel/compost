(* Author: Jackson Warhover 
 * Edited by: Jasper Geer, Roger Burtonpatel 
 *)

module P = Past
module A = Ast
module D = Difflist

module Prim = Primitives

module S = Set.Make(String)
module SM = Map.Make(String)

(* to make pretty printing errors nicer later *)
exception RecursiveUse
exception DuplicateGlobal
exception TypeNameUsage
let except_ru c = if c then raise RecursiveUse else ()
let except_dg c = if c then raise DuplicateGlobal else ()
let except_tnu c = if c then raise TypeNameUsage else ()

let fold_prim l (n, _) = S.add n l
let vb  = ref (SM.empty)                                            (* val binding *)
let gbs = ref (List.fold_left fold_prim S.empty Prim.primitives)    (* global binding set *)
let lbs = ref (S.empty)                                             (* local binding set *)
let dts = ref (List.fold_left fold_prim S.empty Prim.primitive_tys) (* datatype set *)
let use = ref (S.empty)                                             (* use set *)

(* lbr: local bindings (recursive) - locals bound in this context *)
(* pattern: Ast - pattern * expr list *)
(* return: Past - pattern * expr list *)
let rec apes_to_ppes lbr = function
    [] -> []
  | ((A.Pattern (n, ns), e) :: pes) ->
    lbs := List.fold_right (fun n2 lbs ->
      let () = except_tnu (S.mem n2 !dts) in
      S.add n2 lbs
    ) ns !lbs ;
    let lbr2 = List.fold_right S.add ns lbr in
    (A.Pattern(n, ns), ae_to_pe lbr2 e) :: (apes_to_ppes lbr pes)
  | ((A.WildcardPattern, e) :: pes) ->
    (A.WildcardPattern, ae_to_pe lbr e) :: (apes_to_ppes lbr pes)
  | ((A.Name n, e) :: pes) ->
    let () = except_tnu (S.mem n !dts) in
    lbs := S.add n !lbs ;
    let lbr = S.add n lbr in 
    (A.Name n, ae_to_pe lbr e) :: (apes_to_ppes lbr pes)

(* lbr: local bindings (recursive) - locals bound in this context *)
(* pattern: Ast - expr *)
(* return: Past - expr *)
and ae_to_pe lbr = function
  | A.Begin([]) -> P.Literal(A.UnitLit)
  | A.Begin([e]) -> ae_to_pe lbr e
  | A.Begin(e :: es) -> P.Let(Freshnames.fresh_name (), ae_to_pe lbr e, ae_to_pe lbr (A.Begin(es)))
  | A.Let([], e) -> ae_to_pe lbr e
  | A.Let(((abn, abe) :: abs), e) ->
    let () = except_tnu (S.mem abn !dts) in
    lbs := S.add abn !lbs ;
    let lbr2 = S.add abn lbr in
    P.Let(abn, ae_to_pe lbr abe, ae_to_pe lbr2 (A.Let(abs, e)))
  | A.Literal(l) -> P.Literal(l)
  | A.NameExpr(n) ->
    (* if S.mem n !dts then raise TypeNameUsage else (* not really necessary *) *)
    lbs := S.add n !lbs ;
    if (not (S.mem n lbr)) && (SM.mem n !vb)
    then SM.find n !vb
    else P.NameExpr(n)
  | A.Case(e, pes) -> P.Case(ae_to_pe lbr e, apes_to_ppes lbr pes)
  | A.If(e1, e2, e3) -> P.If(ae_to_pe lbr e1, ae_to_pe lbr e2, ae_to_pe lbr e3)
  | A.Apply(e, es) -> P.Apply(ae_to_pe lbr e, List.map (ae_to_pe lbr) es)
  | A.Dup(n) -> 
    (* if S.mem n !dts then raise TypeNameUsage else (* not really necessary *) *)
    P.Dup(n)

(* use_r: use statements (recursive) - checking for recursive use *)
(* pattern: Ast - def *)
(* return: Past - def difflist *)
let rec ad_to_pdl use_r = function
  | A.Use(filename) ->
    let () = except_ru (S.mem filename use_r) in
    if S.mem filename !use then D.empty else
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.program Scanner.token lexbuf in
    use := S.add filename !use ;
    let use_r = S.add filename use_r in
    adl_to_pdl ast use_r
  | A.Val(n, e) -> 
    let () = except_dg (SM.mem n !vb ) in
    let () = except_dg (S.mem n !gbs) in
    let () = except_dg (S.mem n !dts) in
    let pe = ae_to_pe S.empty e in
    vb := SM.add n pe !vb ;
    D.empty
  | A.Define(n, ns, e) -> 
    let () = except_dg (SM.mem n !vb ) in
    let () = except_dg (S.mem n !gbs) in
    let () = except_dg (S.mem n !dts) in
    gbs := S.add n !gbs ;
    lbs := List.fold_right (fun n2 lbs ->
      let () = except_tnu (S.mem n2 !dts) in
      S.add n2 lbs
    ) ns !lbs ;
    let lbr = List.fold_right S.add ns S.empty in
    let pe = ae_to_pe lbr e in
    D.singleton (P.Define(n, ns, pe))
  | A.Datatype(n, ntss) -> 
    let () = except_dg (SM.mem n !vb ) in
    let () = except_dg (S.mem n !gbs) in
    let () = except_dg (S.mem n !dts) in
    let () = except_tnu (S.mem n !lbs) in
    dts := S.add n !dts ;
    gbs := List.fold_right (fun (n2, _) gbs ->
        let () = except_dg (SM.mem n2 !vb ) in
        let () = except_dg (S.mem n2 gbs) in
        let () = except_dg (S.mem n2 !dts) in
        S.add n2 gbs
    ) ntss !gbs ;
    D.singleton (P.Datatype(n, ntss))
  | A.TyAnnotation(n, t) -> D.singleton (P.TyAnnotation(n, t)) (* TODO? *)

(* use_r: use statements (recursive) - checking for recursive use *)
(* pdl: Past - def difflist (FOLD ACCUM) *)
(* ad: Ast - def (FOLD LIST ELEM) *)
(* return: Past - def difflist (FOLD ACCUM) *)
and fold_adl_to_pdl use_r pdl ad = D.cons pdl (ad_to_pdl use_r ad)

(* adl: Ast - def list *)
(* use_r: use statements (recursive) - checking for recursive use *)
(* return: Past - def difflist *)
and adl_to_pdl adl use_r = 
  List.fold_left (fold_adl_to_pdl use_r) D.empty adl

(* ENTRY POINT FROM COMPOST.ML *)
(* adeflist: Ast - def list *)
let preprocess adeflist = D.tolist (adl_to_pdl adeflist S.empty)
