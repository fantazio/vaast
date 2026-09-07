let [| |] = [| |]
let [| 42; 0 |] = [| 1; 2; 3 |]

#if OCAML_VERSION < (5, 4, 0)
#elif OCAML_VERSION >= (5, 4, 0)
(* extension introduced in OCaml 5.4 (see
   https://github.com/ocaml/ocaml/pull/13097)
*)
let [| |] : _ iarray = [| |]
let [| 42; 0 |] : _ iarray = [| 1; 2; 3 |]
#endif
