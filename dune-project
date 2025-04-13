(lang dune 3.17)

(name gullwing)

(generate_opam_files true)

(source
 (github jefremof/ocaml-stack-based-machine))

(authors "Mark Efremov jefremovmark@gmail.com")

(maintainers "Mark Efremov jefremovmark@gmail.com")

(license MIT)

(package
 (name gullwing)
 (synopsis "OCaml implementation of the simple stack-based machine")
 (description "OCaml implementation of the simple stack-based machine. Based on Charles Eric LaForest's Gullwing and heavily inspired by his book \"Second-Generation Stack Computer
Architecture\"")
 (depends
  ocaml
  core
  ppx_deriving
  ppx_fields_conv
  ppx_assert
  ppx_sexp_conv
  ppx_inline_test
  ppx_jane
  sexplib)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
