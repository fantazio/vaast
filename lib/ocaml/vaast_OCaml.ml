(** compiler-libs' definitions *)

module Typedtree = struct
  include Typedtree

  (* types introduced in subsequent versions of OCaml *)

  #if OCAML_VERSION < (5, 2, 0)
  type function_param = {
    fp_arg_label: Asttypes.arg_label;
    fp_param: Ident.t;
    fp_partial: partial;
    fp_kind: function_param_kind;
    fp_newtypes: string Asttypes.loc list;
    fp_loc: Location.t;
  }

  and function_param_kind =
    | Tparam_pat of pattern
    | Tparam_optional_default of pattern * expression

  and function_body =
    | Tfunction_body of expression
    | Tfunction_cases of {
          cases: value case list;
          partial: partial;
          param: Ident.t;
          loc: Location.t;
          exp_extra: exp_extra option;
          attributes: attributes;
        }
  #elif OCAML_VERSION >= (5, 2, 0)
  #endif

  #if OCAML_VERSION < (5, 4, 0)
  type ('a, 'b) arg_or_omitted =
    | Arg of 'a
    | Omitted of 'b

  and apply_arg = (expression, unit) arg_or_omitted
  #elif OCAML_VERSION >= (5, 4, 0)
  #endif
end

module Types = struct
  include Types

  (* types introduced in subsequent versions of OCaml *)

  #if OCAML_VERSION < (5, 4, 0)
  type package = {
    pack_path : Path.t;
    pack_cstrs : (string list * type_expr) list;
  }
  #elif OCAML_VERSION >= (5, 4, 0)
  #endif
end

module Asttypes = struct
  #if OCAML_VERSION < (5, 3, 0)
  (* Module Asttypes did not have an implementation until OCaml 5.3,
     so it cannot be linked until then (see
     https://github.com/ocaml/ocaml/pull/13191)

     The code below is derived from
     https://github.com/ocaml/ocaml/blob/5.2.0/parsing/asttypes.mli
  *)
  type constant = Asttypes.constant =
    | Const_int of int
    | Const_char of char
    | Const_string of string * Location.t * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint

  type rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive

  type direction_flag = Asttypes.direction_flag = Upto | Downto

  (* Order matters, used in polymorphic comparison *)
  type private_flag = Asttypes.private_flag = Private | Public

  type mutable_flag = Asttypes.mutable_flag = Immutable | Mutable

  type virtual_flag = Asttypes.virtual_flag = Virtual | Concrete

  type override_flag = Asttypes.override_flag = Override | Fresh

  type closed_flag = Asttypes.closed_flag = Closed | Open

  type label = Asttypes.label (* = string *)

  type arg_label = Asttypes.arg_label =
    | Nolabel
    | Labelled of string (** [label:T -> ...] *)
    | Optional of string (** [?label:T -> ...] *)

  type 'a loc = 'a Asttypes.loc = {
    txt : 'a;
    loc : Location.t;
  }


  type variance = Asttypes.variance =
    | Covariant
    | Contravariant
    | NoVariance

  type injectivity = Asttypes.injectivity =
    | Injective
    | NoInjectivity

  #elif OCAML_VERSION >= (5, 3, 0)
  include Asttypes
  #endif

  (* types introduced in subsequent versions of OCaml *)

  #if OCAML_VERSION < (5, 4, 0)
  type atomic_flag = Nonatomic | Atomic
  #elif OCAML_VERSION >= (5, 4, 0)
  #endif
end

#if OCAML_VERSION < (5, 2, 0)
(* Module introduced in 5.2 by a refactor (see
   https://github.com/ocaml/ocaml/pull/12608/)
*)
module Value_rec_types = struct
  type recursive_binding_kind =
    | Static
    | Dynamic
end
#elif OCAML_VERSION >= (5, 2, 0)
module Value_rec_types = Value_rec_types
#endif

#if OCAML_VERSION < (5, 4, 0)
(* Module introduced in 5.4 by a refactor (see
   https://github.com/ocaml/ocaml/pull/13466)
*)
module Data_types = struct
  type constructor_description = Types.constructor_description
  type constructor_tag = Types.constructor_tag
  type label_description = Types.label_description
end
#elif OCAML_VERSION >= (5, 4, 0)
module Data_types = Data_types
#endif
