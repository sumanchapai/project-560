# Refinement Type Checker for OCaml

## Steps to Run:

Install dependencies

```/bin/bash
opam install . --deps-only
```

We use OCaml version 5.2.1. If you are on any other version, you may switch using:

```/bin/bash
opam switch create 5.2.1
```

One of the dependencies is `z3` which will require `python3` to be installed on
your system as well as `pkg-config` and maybe, `libgmp-dev`.

##  Run tests

```/bin/bash
opam exec -- dune runtest
```
## Run Demos

To run the type checker with some demo examples:

```/bin/bash
make demo
```

To run the type checker with your example file:

```/bin/bash
make custom
```
Then provide the file path to your OCaml program. You may refer to the "examples" folder for some examples.
