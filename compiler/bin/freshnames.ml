(* God I hate global mutable state but it is so very useful here *)
let counter = ref 0

let fresh_name () = begin counter := !counter + 1 ; Int.to_string !counter end
