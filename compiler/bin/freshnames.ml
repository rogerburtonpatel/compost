(* God I hate global mutable state but it is so very useful here *)
let counter = ref 1

let fresh_name = begin incr counter ; Int.to_string !counter end
