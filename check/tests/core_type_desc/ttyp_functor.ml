#if OCAML_VERSION < (5, 5, 0)
#elif OCAML_VERSION >= (5, 5, 0)
(* syntax introduced in OCaml 5.5 (see
   https://github.com/ocaml/ocaml/pull/13275)
*)
module type S = sig type t end
type t_no_label = (module M : S) -> M.t
type t_with_label = m:(module M : S) -> M.t
#endif
