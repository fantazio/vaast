module _ : functor (_ : sig val x : int end) -> sig end =
  functor (_ : sig end) -> struct end

module _ : functor (_ : sig end) -> sig end =
  functor (_ : sig end) -> struct let x = 0 end

module _ : functor (_ : sig val x : int end) -> sig end =
  functor (_ : sig end) -> struct let x = 0 end

module F (_ : sig end) = struct let x = 0 end
module _ = (F : functor (_ : sig val x : int end) -> sig end)
module _ : functor (_ : sig val x : int end) -> sig end = F

