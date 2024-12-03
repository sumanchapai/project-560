(* test_my_module.ml *)
open OUnit2

(* Make success/failure result types *)
let make_success _ = Ok ()
let make_error err_string = Error err_string

let create_arith_eval_suite name cases : test =
  (* Helper function to create a test case *)
  let create_test_case index (program, expected_result) =
    (* The test function to execute *)
    let test_fun _ =
      let actual_result = Checker.Helpers.arith_eval program in
      assert_equal actual_result expected_result
    in
    (* Test case name is derived from index *)
    Printf.sprintf "Test case %d" index >:: test_fun
  in
  (* Create a list of tests by mapping over the cases *)
  let tests = List.mapi create_test_case cases in
  (* Return a test suite with the given name and test cases *)
  name >::: tests


(* `create_suites` generates a suite of tests for the provided test cases. *)
let create_refinement_suites name cases : test =
  (* Helper function to create a test case *)
  let create_test_case index (program, expected_result) =
    (* The test function to execute *)
    let test_fun _ =
      let actual_result = Checker.Type_checker.type_check program in
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

let refinement_type_check_suite =
  create_refinement_suites "basics" [

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
      let a : int[@refinement (v, v > 0)] = 1 + 1
      |},
      make_success ()
    );

    (
      {|
      let a : int[@refinement (v, v > 0)] = 1 * 5
      |},
      make_success ()
    );

    (
      {|
      let a : int[@refinement (v, v > 0)] = 1 * (5 - 3)
      |},
      make_success ()
    );

    (
      {|
      let a : int[@refinement (v, v > 0)] = 1 * (3 - 5)
      |},
      make_error "expected v > 5; found v = -2"
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

(* Test suite for arithmetic eval function *)
let arith_eval_check_suite =
  create_arith_eval_suite "basics" [
    ("1", 1);
    ("1 + 1", 2);
    ("1 + 5", 6);
    ("(1 + 5) * 10", 60);
    ("-3 + 5 * 2", -4);
  ]


let () = 
  let all_tests = 
    "all_tests" >::: [
      refinement_type_check_suite;
      arith_eval_check_suite;
    ]
  in
  run_test_tt_main all_tests
