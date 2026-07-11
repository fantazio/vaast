module type S = sig end

let _ = (module struct end : S)

let _ : (module S) = (module struct end)
