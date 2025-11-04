(lang dune 3.20)

(name alua)
(using menhir 3.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name alua)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml ppx_inline_test)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
(formatting
 (enabled_for ocaml))
