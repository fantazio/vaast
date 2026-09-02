(** Types used for version selection. The types [('until, 'since) ocaml_XXX]
    are used to offer version-dependent alternatives on specific elements.
    The ['until] type is used in OCaml < XXX, and the ['since] is used in
    OCaml >= XXX.
*)

(** Used within [ocaml_XXX] types to indicate that an element is not available
 until or since the corresponding version.
*)
type not_available = NA

type ('until, 'since) ocaml_500 =
  | Until_500 of 'until (** type until 5.0.0 excluded *)
  | Since_500 of 'since (** type since 5.0.0 included *)

type ('until, 'since) ocaml_510 =
  | Until_510 of 'until (** type until 5.1.0 excluded *)
  | Since_510 of 'since (** type since 5.1.0 included *)

type ('until, 'since) ocaml_520 =
  | Until_520 of 'until (** type until 5.2.0 excluded *)
  | Since_520 of 'since (** type since 5.2.0 included *)

type ('until, 'since) ocaml_530 =
  | Until_530 of 'until (** type until 5.3.0 excluded *)
  | Since_530 of 'since (** type since 5.3.0 included *)

