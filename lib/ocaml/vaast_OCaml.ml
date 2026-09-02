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
end

#if OCAML_VERSION < (5, 2, 0)
(* Module introduced in 5.2 by a refactor (see
   https://github.com/ocaml/ocaml/pull/12608/)
*)
module Value_rec_types =
  struct
    type recursive_binding_kind =
      | Static
      | Dynamic
  end
#elif OCAML_VERSION >= (5, 2, 0)
module Value_rec_types = Value_rec_types
#endif
