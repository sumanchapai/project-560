(* Fail case *)
let min : int = 0
let max : int = 100
let base : int = 100
let a : int[@refinement (v, v > min && v < max)] = base + 100

