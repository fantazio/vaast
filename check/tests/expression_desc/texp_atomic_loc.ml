#if OCAML_VERSION < (5, 4, 0)
#elif OCAML_VERSION >= (5, 4, 0)
type t = { mutable x : unit [@atomic] }
let _ =
  let r = { x = () } in
  (* extension introduced in OCaml 5.4 (see
     https://github.com/ocaml/ocaml/pull/13404)
  *)
  [%ocaml.atomic.loc r.x]
#endif
