let x = ()

module type S = sig end

let _ = let (module M) = (module struct end : S) in ()
