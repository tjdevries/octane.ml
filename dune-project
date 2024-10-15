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

;; Until published, we can pin directly.
(pin
 (url "git+https://github.com/dbcaml/dbcaml.git")
 (package (name dbcaml)))

(pin
 (url "git+https://github.com/dbcaml/dbcaml.git")
 (package (name dbcaml-driver-postgres)))

(pin
 (url "git+https://github.com/dbcaml/dbcaml.git")
 (package (name serde-postgres)))

(pin
 (url "git+https://github.com/riot-ml/riot.git")
 (package (name riot)))

(package
 (name oql)
 (synopsis "OCaml Query Language")
 (description "OCaml Query Language - very cool")
 (depends 
   ;; OCaml deps
   (ocaml (>= 5.0)) dune 
   ;; Parsing deps
   menhirLib
   dbcaml
   (mirage-crypto-rng (>= "1"))
   fmt ppx_deriving ppx_jane)
 (tags (topics "oql")))

(package
 (name octane)
 (synopsis "The fastest, hottest")
 (description "The fastest, hottest")
 (tags (topics "sql" "database" "query builder"))
 (depends
   (ocaml (>= 5.0)) dune 
   fmt bos base ppx_deriving
   ;; Riot suite
   dbcaml dbcaml-driver-postgres riot serde serde_derive serde-postgres
   ;; Local projects
   oql ppx_octane))

(package
 (name ppx_octane)
 (synopsis "PPX for deriving octane tables, queries and more")
 (description "PPX for deriving octane tables, queries and more. Includes %query, %table")
 (depends
   (ocaml (>= 5.0)) dune 
   ppxlib fmt bos base oql))

