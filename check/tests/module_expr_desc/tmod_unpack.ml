module type S = sig end

module _ = (val (module struct end : S))

module _ = (val (module struct end) : S)
