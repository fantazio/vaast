#if OCAML_VERSION < (5, 5, 0)
#elif OCAML_VERSION >= (5, 5, 0)
(* syntax introduced in OCaml 5.5 (see
   https://github.com/ocaml/ocaml/pull/13712)
*)
type t = external "t"
#endif
