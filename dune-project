(lang dune 3.20)

(name vaast)

(generate_opam_files true)

(version 0.0.0)

(maintainers
 "Corentin De Souza <corentin@fantazio.eu>"
)

(authors
 "Corentin De Souza <corentin@fantazio.eu>"
)

(source (github fantazio/vaast))


(package
 (name vaast)
 (synopsis "Version-agnostic Typedtree for OCaml")
 (license MIT)
 (depends
  (ocaml (and (>= 4.14) (< 5.6)))
  cppo
 )
)
