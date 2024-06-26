(lang dune 3.14)

(name octane)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name octane)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(package
 (name oql)
 (synopsis "OCaml Query Language")
 (description "OCaml Query Language - very cool")
 (depends ocaml dune menhirlib fmt ppx_deriving)
 (tags (topics "oql")))

(package
 (name ppx_query)
 (synopsis "Cool ppxs for queries")
 (description "Cool ppxs for queries")
 (depends ocaml dune ppxlib)
 (tags
  (topics "to describe" your project)))

