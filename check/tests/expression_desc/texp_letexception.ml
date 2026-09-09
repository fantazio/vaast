let _ = let exception E in ()

let _ = let exception E of int in ()

let _ = let exception E of int * int in ()

(* Replaced by Texp_struct_item since OCaml 5.5.
   See texp_struct_item.ml
*)
