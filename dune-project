(lang dune 3.14)

(name octane)
(generate_opam_files true)

(using menhir 3.0)

(source
 (github tjdevries/octane.ml))

(license MIT)

(authors "TJ DeVries")
(maintainers "TJ DeVries")

; (documentation https://url/to/documentation)

(package
 (name octane)
 (synopsis "The fastest, hottest")
 (description "The fastest, hottest")
 (depends 
   ocaml 
   base
   core
   ppx_jane
   dune
   riot
   ppx_octane
   ppx_deriving)
 (tags
  (topics "sql" "database" "query builder")))

(package
 (name oql)
 (synopsis "OCaml Query Language")
 (description "OCaml Query Language - very cool")
 (depends 
   ocaml 
   dune 
   menhirLib 
   menhir
   fmt 
   ppx_jane
   ppxlib
   ppx_deriving)
 (tags (topics "oql")))

(package
 (name ppx_octane)
 (synopsis "PPX for deriving octane tables, queries and more")
 (description "PPX for deriving octane tables, queries and more. Includes %query, %table")
 (depends
   ocaml 
   dune 
   ppxlib 
   oql 
   base 
   fmt 
   ppx_deriving 
   serde_derive 
   bos)
 (tags
  (topics "to describe" your project)))

