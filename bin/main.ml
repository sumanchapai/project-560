let input_file = "test/test_project560.ml" in 
let program = Ocaml_parser.Frontend.parse ~sourcefile:input_file in
let _ = Ocaml_parser.Printast.structure 0 Format.std_formatter program in ()
;;

