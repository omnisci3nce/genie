(lang dune 3.14)

(name genie)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name genie)
 (synopsis "Simple GUI framwork in OCaml")
 (description "A longer description")
 (depends ocaml dune bindgen containers ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
