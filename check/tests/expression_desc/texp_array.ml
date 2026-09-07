let _ = [| |]
let _ = [| 1; 2; 3 |]
#if OCAML_VERSION < (5, 4, 0)
#elif OCAML_VERSION >= (5, 4, 0)
(* extension introduced in OCaml 5.4 (see
   https://github.com/ocaml/ocaml/pull/13097)
*)
let _ : _ iarray = [| |]
let _ : _ iarray = [| 1; 2; 3 |]
#endif
