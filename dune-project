(lang dune 3.14)

(name GitGud)

(generate_opam_files true)

(source
 (github Aimeric1/GitGud))

(authors "Alice Tosel <alice.tosel@ens.psl.eu>" "Aimeric Duchemin <aimeric.duchemin@ens.psl.eu>")

(license LICENSE)

(package
 (name GitGud)
 (allow_empty)
 (synopsis "Un super git")
 (depends ocaml dune cmdliner sha ocaml-inifiles)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
