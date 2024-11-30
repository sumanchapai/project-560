(lang dune 3.16)

(name refinement_project)

(generate_opam_files true)

(source
 (github sumanchapai/project-560))

(authors "Suman", "Rachit" "Asha")

(maintainers "Suman Chapai <sumanchapai@gmail.com>")

(license MIT)

(documentation https://github.com/sumanchapai/project-560)

(package
 (name refinement_project)
 (synopsis "A framework for refinement types in OCaml.")
 (description "A project that adds refinement type checking using PPX and Z3.")
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.0)))
 (allow_empty)
 )

(package
 (name ppx_refinements)
 (synopsis "A PPX rewriter for refinement types")
 (description "Refinement types PPX rewriter")
 )
