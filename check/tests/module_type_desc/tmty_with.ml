module type S_with_t = sig type t type t_subst end with type t = int

module type S_with_t_subst = sig type t end with type t := int

module type S_with_m = sig module M : sig end end with module M = Stdlib

module type S_with_m_subst = sig module M : sig end end with module M := Stdlib

module type S_with_mt = sig module type S end with module type S = Map.S

module type S_with_mt_subst = sig module type S end with module type S := Map.S

module type S_with_all = sig
  type t
  type t_subst
  module M : sig end
  module M_subst : sig end
  module type S
  module type S_subst
end
  with  type t = int
    and type t_subst := int
    and module M = Stdlib
    and module M_subst := Stdlib
    and module type S = Map.S
    and module type S_subst := Map.S
