#if OCAML_VERSION < (5, 2, 0)
#elif OCAML_VERSION >= (5, 2, 0)
(* syntax introduced in OCaml 5.2 (see
   https://github.com/ocaml/ocaml/pull/12044)
*)
type int_list = List.(int t)
#endif
