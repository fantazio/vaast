let _ = [%extension_constructor Not_found]

type t = ..
type t += Wrap of int

let _ : extension_constructor = [%ocaml.extension_constructor Wrap]
