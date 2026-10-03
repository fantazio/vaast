(** Types used for version selection. The types [('until, 'since) ocaml_XYY]
    are used to offer version-dependent alternatives on specific elements.
    The ['until] type is used in OCaml < X.YY, and the ['since] is used in
    OCaml >= X.YY.
    If the minor is a single digit, then it is prefixed by a 0.
*)

(** Used within [ocaml_XYY] types to indicate that an element is not available
 until or since the corresponding version.
*)
type not_available = NA

type ('until, 'since) ocaml_500 =
  | Until_500 of 'until (** type until 5.0 excluded *)
  | Since_500 of 'since (** type since 5.0 included *)

type ('until, 'since) ocaml_501 =
  | Until_501 of 'until (** type until 5.1 excluded *)
  | Since_501 of 'since (** type since 5.1 included *)

type ('until, 'since) ocaml_502 =
  | Until_502 of 'until (** type until 5.2 excluded *)
  | Since_502 of 'since (** type since 5.2 included *)

type ('until, 'since) ocaml_503 =
  | Until_503 of 'until (** type until 5.3 excluded *)
  | Since_503 of 'since (** type since 5.3 included *)

type ('until, 'since) ocaml_504 =
  | Until_504 of 'until (** type until 5.4 excluded *)
  | Since_504 of 'since (** type since 5.4 included *)

type ('until, 'since) ocaml_505 =
  | Until_505 of 'until (** type until 5.5 excluded *)
  | Since_505 of 'since (** type since 5.5 included *)
