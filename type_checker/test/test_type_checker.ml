(* test_my_module.ml *)
open OUnit2

(* A simple function to be tested *)
let add x y = x + y

(* 
   The following is a mock test suite and it provides a good check of whether
   the testing framework we have is functional

   *)
let suite =
  "Test Suite for add function" >::: [
    "test_addition" >:: 
    fun (_) -> assert_equal 5 (add 2 3)
  ]

let () =
  (* Addition suite *)
  run_test_tt_main suite
