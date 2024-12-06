(* fail case *)

let x : int [@refinement v, v  <= 2] = 1 + 1
let y : bool [@refinement v1, v = false ]= true