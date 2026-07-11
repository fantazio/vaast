type t = { x : int; y : unit }

let { x; y } = { x = 1; y = () }

let {x; _ } = { x = 1; y = () }
