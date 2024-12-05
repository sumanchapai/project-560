(* Creates an executable for the type checker *)
let () =
  (* Ensure the program receives exactly one argument *)
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in

  (* Read the file contents *)
  let file_contents =
    try
      let ic = open_in filename in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      s
    with Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  in

  (* Call the checker on the file contents *)
  Printf.printf "Processing file: %s\n" filename;
  match Checker.Type_checker.type_check file_contents with
    |Ok _ -> 
      Printf.printf "Refinement Type Check Successful\n"
  | Error err -> 
      Printf.printf "Checker checking failed: %s\n" err;
      exit 1


