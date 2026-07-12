module _ () = struct end
module _ = functor () -> struct end

module _ (_ : sig end) = struct end
module _ = functor (_ : sig end) -> struct end

module _ (P : sig end) = P
module _ = functor (P : sig end) -> P

module _ (P : sig end) () = P
module _ = functor (P : sig end) () -> P
module _ = functor (P : sig end) -> functor () -> P
