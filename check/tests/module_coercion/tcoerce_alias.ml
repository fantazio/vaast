module M = struct end

module _ : sig module M : sig end end = struct
  module M = M
end
