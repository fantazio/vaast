1, 2
let _ = (1, 'c', true)

#if OCAML_VERSION < (5, 4, 0)
#elif OCAML_VERSION >= (5, 4, 0)
(* syntax introduced in OCaml 5.4 (see
   https://github.com/ocaml/ocaml/pull/13498)
*)
let _ = (~a:1, ~b:true)
let _ = (1, ~l:true)
#endif
