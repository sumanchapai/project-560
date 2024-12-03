(* test_my_module.ml *)
open OUnit2

(* Make success/failure result types *)
let make_success _ = Ok ()
let make_error err_string = Error err_string


(* `create_suites` generates a suite of tests for the provided test cases. *)
let create_suites name cases : test =
  (* Helper function to create a test case *)
  let create_test_case index (program, expected_result) =
    (* The test function to execute *)
    let test_fun _ =
      let actual_result = Type_checker.type_check program in
      assert_equal ~cmp:( = ) ~printer:(fun r -> match r with
        | Ok _ -> "Success"
        | Error msg -> "Error: " ^ msg
      ) expected_result actual_result
    in
    (* Test case name is derived from index *)
    Printf.sprintf "Test case %d" index >:: test_fun
  in
  (* Create a list of tests by mapping over the cases *)
  let tests = List.mapi create_test_case cases in
  (* Return a test suite with the given name and test cases *)
  name >::: tests

let basic_test_suites =
  create_suites "basics" [

    (
      {|
      let a : int[@refinement (v, v > 0)] = 5
      |},
      make_success ()
    );

    (
      {|
      let a : int[@refinement (v, v > 5)] = 5
      |},
      make_error "expected v > 5; found v = 5"
    );

    (
      {|
      let sum_1 a b : int[@refinement (v, v >= a && v >= b)] = a + b
      |},
      make_success ()
    );

    (
      {|
      type nat = int[@refinement (v, v > 0)]
      let a : nat = 1
      |},
      make_success ()
    );

    (
      {|
      type nat = int[@refinement (v, v > 0)]
      let a : nat = -1
      |},
      make_error ("expected a > 0; found a = -1")
    );

  ]

let () = 
run_test_tt_main basic_test_suites
