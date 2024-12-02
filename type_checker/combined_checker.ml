(* combined_checker.ml *)
open Ppxlib
open Type_checker

let process_file filename =
  let ic = open_in filename in
  let length = in_channel_length ic in
  let input_code = really_input_string ic length in
  close_in ic;

  (* Simulate PPX transformation *)
  let transformed_code = input_code (* Replace with actual transformation logic *) in

  (* Type check the transformed code *)
  let result = check_type transformed_code in
  Printf.printf "Type checking result: %b\n" result;
  result

let () =
  if Array.length Sys.argv < 2 then
    prerr_endline "Usage: combined_checker <file>"
  else
    let filename = Sys.argv.(1) in
    ignore (process_file filename)
