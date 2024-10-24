We are using Zhe's [ocaml_parser](https://github.com/zhezhouzz/ocaml_parser/)
for parsing. We're just copying the repo and placing inside `lib/ocaml_parser`.

We have an executable inside `bin/main.ml` that will call this library to do
things. For example, with the contents of `bin/main.ml` as follows:

```
let program = Ocaml_parser.Frontend.parse_string "1 + 2"
let () = Ocaml_parser.Printast.structure 0 Format.std_formatter program
```

if you run `dune exec bin/main.exe`, it will print the AST.

## Development

You should be able to just clone the repo and run the dune commands. I use the
watch mode which just rebuilds and rexecutes when you make changes:
`dune exec bin/main.exe -w`
