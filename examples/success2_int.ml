(* sucess case *)
let a: int = 10
let b: int = 5
let c: int [@refinement v, v > a && v > b] = a + b
let res : bool [@refinement v1, true] = c > b