let program = Ocaml_parser.Frontend.parse_string "1 + 2"
let () = Ocaml_parser.Printast.structure 0 Format.std_formatter program
