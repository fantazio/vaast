let _ = let exception E in ()

let _ = let exception E of int in ()

let _ = let exception E of int * int in ()
