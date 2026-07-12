module _ = struct let x = 0 let x = 1 end

module _ : sig end = struct end

module _ = (struct end : sig end)
