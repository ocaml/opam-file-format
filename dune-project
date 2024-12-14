(lang dune 3.17)
(name opam-file-format)
(version 2.2.0~alpha1)
(using menhir 3.0)
(generate_opam_files true)

(maintainers "Louis Gesbert <louis.gesbert@ocamlpro.com>")
(authors "Louis Gesbert <louis.gesbert@ocamlpro.com>")
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(homepage "https://opam.ocaml.org")
(source (github ocaml/opam-file-format))

(package
 (name opam-file-format)
 (synopsis "Parser and printer for the opam file syntax")
 (depends
  (ocaml (>= 4.02))
  (menhir (>= 20211230))
  (alcotest (and :with-test (>= 0.4.8)))
  (fmt :with-test)))
