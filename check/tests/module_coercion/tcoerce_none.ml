module _ : sig end = struct end

module type S = sig val x : int end
module _ : S = struct let x = 0 end
