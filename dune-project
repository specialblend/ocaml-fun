(lang dune 3.10)

(name fun)

(generate_opam_files true)

(source
 (github specialblend/ocaml-fun))

(authors "cj@cavemon.dev")

(maintainers "cj@cavemon.dev")

(license MIT)

(documentation https://github.com/specialblend/ocaml-fun/blob/main/README.md)

(package
 (name fun)
 (synopsis "Functional helpers for ocaml")
 (description "Functional helpers for ocaml")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
