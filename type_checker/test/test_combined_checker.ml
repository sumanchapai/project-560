(* test_combined_checker.ml *)
open Alcotest

(* Helper function to simulate a test program file *)
let write_sample_program filename content =
  let oc = open_out filename in
  Printf.fprintf oc "%s" content;
  close_out oc

(* Test function for the combined checker *)
let test_combined_checker () =
  (* Sample program with a refinement type *)
  let sample_program = {|
    let x : [%refinement (v : int, v > 0)] = 5
  |} in
  let test_file = "sample_test.ml" in
  write_sample_program test_file sample_program;

  (* Run the combined checker on the test file *)
  let cmd = "./combined_checker " ^ test_file in
  let status = Sys.command cmd in

  (* Check that the combined checker succeeds *)
  check bool "Expected type check to pass" true (status = 0)

(* Test suite *)
let () =
  let open Alcotest in
  run "Type Checker Tests" [
    "Combined Checker", [ test_case "Test 1" `Quick test_combined_checker` ];
  ]

