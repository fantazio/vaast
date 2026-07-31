try () with
| Not_found -> ()
| _ -> ()
#if OCAML_VERSION < (5, 3, 0)
#elif OCAML_VERSION >= (5, 3, 0)
(* syntax introduced in OCaml 5.3 (see
   https://github.com/ocaml/ocaml/pull/12309)
*)
| effect _, _ -> ()
| effect _, k -> Effect.Deep.discontinue k Exit
#endif
