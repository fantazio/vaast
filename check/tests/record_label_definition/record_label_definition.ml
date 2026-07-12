type t = { kept: int; overriden: int }

let _ =
  let r = { kept = 0; overriden = 1 } in
  { r with overriden = 2 }
