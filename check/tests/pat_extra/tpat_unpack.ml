module type S = sig end

module M = struct end

let (module _) = (module M : S)

let (module _ : S) = (module M)
