let a : int = 1 * (5 - 3)
let b : int = 1 * (3 - 5)
let c : int [@refinement v, v < a && v < b]= a * b