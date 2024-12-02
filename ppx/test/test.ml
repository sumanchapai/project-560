let a : [%refinement (v : int), v > 0] = 5
let b = 10
let sum_1 a b : [%refinement (v: int), v >= a && v >= b] = a+b
let sum_2 (a: int) (b: int) : [%refinement (v: int), v >= a && v >= b] = a+b
type positive = [%refinement (v : int), v > 0]
type nonnegative = [%refinement (v : int), v >= 0]
let sum_3 (a: positive) (b: positive) : [%refinement (v: positive), v >= a && v >= b] = a+b
