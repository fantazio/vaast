let (let+) x f = f x
let (and+) x y = (x, y)

let _ =
  let+ () = () in
  let+ () = ()
  and+ () = () in
  let _x = () in
  let+ _x in
  ()
