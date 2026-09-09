#if OCAML_VERSION < (5, 5, 0)
(* see texp_letmodule.ml, texp_letexception.ml and texp_open.ml
   for OCaml < 5.5 constructs
*)
#elif OCAML_VERSION >= (5, 5, 0)
(* syntax extended in OCaml 5.5 (see
   https://github.com/ocaml/ocaml/pull/13835)
*)
let _ =
  (* let 1 in (* Not allowed *) *)
  (* let let v = () in (* Not allowed *) *)
  let external x : _ -> _ = "" in
  let type nonrec t1 = ..
  and             t2
  in
  let type t1 += Foo in
  let exception E in
  let module M = struct end in
  let module rec M1 : sig end = struct end
  and            M2 : sig end = struct end
  in
  let module type S in
  let open M in
  let class c1 = object end
  and       c2 = object end
  in
  let class type ct2 = object end
  and            ct3 = object end
  in
  (* let include M in (* Not allowed *) *)
  let [@@@inline] in
  ()
#endif
