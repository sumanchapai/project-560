let a : ((int)[@refinement let v = v > 0]) = 5
let b = 10
let sum_1 a b = (a + b : ((int)[@refinement let v = (v >= a) && (v >= b)]))
let sum_2 (a : int) (b : int) =
  (a + b : ((int)[@refinement let v = (v >= a) && (v >= b)]))
type positive = ((int)[@refinement let v = v > 0])
type nonnegative = ((int)[@refinement let v = v >= 0])
let sum_3 (a : positive) (b : positive) =
  (a + b : ((positive)[@refinement let v = (v >= a) && (v >= b)]))
