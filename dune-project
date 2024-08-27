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
 (depends ocaml dune)
 (tags
  (topics "sql" "database" "query builder")))

(package
 (name oql)
 (synopsis "OCaml Query Language")
 (description "OCaml Query Language - very cool")
 (depends
   ocaml dune
   bos core fmt ppx_deriving ppx_jane

   (ppx_inline_tet :with-test)
   (ppx_expect :with-test)

   ;; Vendored package deps
   (rio (>= "0.0.8"))
   (spices :with-test)
   (qcheck :with-test)

   (bytestring (>= "0.0.1"))
   castore
   (config (>= "0.0.1"))
   (gluon (>= "0.0.1"))
   (mdx (and :with-test (>= "2.3.1")))
   (mirage-crypto (>= "0.11.2"))
   (mirage-crypto-rng (>= "0.11.2"))
   (mtime (>= "2.0.0"))
   (ocaml (>= "5.1"))
   (odoc (and :with-doc (>= "2.2.2")))
   (ptime (>= "1.1.0"))
   (randomconv (= "0.2.0"))
   (rio (>= "0.0.1"))
   (telemetry (>= "0.0.1"))
   (tls (= "0.17.3"))
   (uri (>= "4.4.0"))
   (x509 (and :with-test (>= "0.16.5")))

   uri
   (alcotest :with-test)
   cryptokit

   ;; Actual Deps
   menhir) 

   
   
 (tags (topics "oql")))

(package
 (name ppx_octane)
 (synopsis "PPX for deriving octane tables, queries and more")
 (description "PPX for deriving octane tables, queries and more. Includes %query, %table")
 (depends ocaml dune ppxlib oql)
 (tags
  (topics "to describe" your project)))

