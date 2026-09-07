type t_no_label = (int * unit)

#if OCAML_VERSION < (5, 4, 0)
#elif OCAML_VERSION >= (5, 4, 0)
(* syntax introduced in OCaml 5.4 (see
   https://github.com/ocaml/ocaml/pull/13498)
*)
type t_with_label = (a:int * b:unit)
type t_mixed = (int * l:unit)
#endif
