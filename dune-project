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
 (package (name silo)))

(pin
 (url "git+https://github.com/dbcaml/dbcaml.git")
 (package (name serde-postgres)))

(pin
 (url "git+https://github.com/serde-ml/serde.git")
 (package (name serde)))

(pin
 (url "git+https://github.com/serde-ml/serde.git")
 (package (name serde_json)))

(pin
 (url "git+https://github.com/serde-ml/serde.git")
 (package (name serde_derive)))

(pin
 (url "git+https://github.com/riot-ml/riot.git")
 (package (name riot)))

(package
 (name oql)
 (synopsis "OCaml Query Language")
 (description "OCaml Query Language - very cool")
 (depends 
   ;; OCaml deps
   (ocaml (>= 5.0)) dune ocamlformat
   ;; Parsing deps
   menhir menhirLib
   dbcaml dbcaml-driver-postgres silo bos
   serde serde_derive serde-postgres
   (mirage-crypto-rng (>= "1"))
   fmt core ppx_deriving ppx_jane)
 (tags (topics "oql")))

(package
 (name octane)
 (synopsis "The fastest, hottest")
 (description "The fastest, hottest")
 (tags (topics "sql" "database" "query builder"))
 (depends
   (ocaml (>= 5.0)) dune 
   fmt bos core ppx_deriving ppx_expect
   ;; Riot suite
   dbcaml dbcaml-driver-postgres riot serde serde_derive serde-postgres silo
   ;; Local projects
   oql ppx_octane))

(package
 (name ppx_octane)
 (synopsis "PPX for deriving octane tables, queries and more")
 (description "PPX for deriving octane tables, queries and more. Includes %query, %table")
 (depends
   (ocaml (>= 5.0)) dune 
   ppxlib fmt bos core oql))

