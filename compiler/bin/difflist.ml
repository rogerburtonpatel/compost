(* Author: Jackson Warhover *)

let singleton x xs = x :: xs
let empty tail = tail
let tolist f = f []
let cons f g x = f (g x)
