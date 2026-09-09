let _ = let module _ = struct end in ()

let _ = let module M = struct end in ()

(* Replaced by Texp_struct_item since OCaml 5.5.
   See texp_struct_item.ml
*)
