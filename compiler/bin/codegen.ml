module L = Llvm
module M = Mast

let ctx = L.create_context

let rec expr builder functions locals ctx = function
  | M.Literal l -> match l with
    | _ -> ()
