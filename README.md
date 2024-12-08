#Refinement Type Checker for OCaml

## Steps to Run:

Install dependencies

```/bin/bash
opam install . --deps-only
```

One of the dependencies is `z3` which will require `python3` to be installed on
your system as well as `pkg-config` and maybe, `libgmp-dev`.

Run tests

```/bin/bash
opam exec -- dune runtest
```

To run the type checker with some demo examples:

```/bin/bash
make demo
```

To run the type checker with your example file:

```/bin/bash
make custom
```
Then provide the file path
