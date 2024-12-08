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

Demo:

```/bin/bash
make demo
```
