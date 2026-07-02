(** Multi-version compatbile representation of the OCaml Typedtree.

    Its aim is to provide a uniform representation of the Typedtree accross all
    compiler versions, enabling users of the Typedtree to support the
    corresponding OCaml versions without the hassle of manual management (e.g.
    via dedicated branches or cppo).
    Its interface may break when a new OCaml minor version is released.
    However, this breakage should be more lightweight than that of the compiler's
    Typedtree.

    Code is heavily inspired by the OCaml Typedtree itself.
    See {:https://github.com/ocaml/ocaml/blob/4.14.4/typing/typedtree.mli}
    for the original copyright and license..
*)

(* Value expressions for the core language *)

(** [partial] indicates if all pattern cases are accounted for or not
    e.g. in [match expr with cases] and [function cases]
*)
type partial = Partial | Total

(** {1 Extension points} *)

(** [[@attribute]] constructs*)
type attribute = Parsetree.attribute
type attributes = attribute list

(** {1 Core language} *)

(** [value] patterns match the result of a computation *)
type value = Typedtree.value = Value_pattern

(** [computation] patterns handle the effects of a computation *)
type computation = Typedtree.computation = Computation_pattern

type pattern = value general_pattern
and 'k general_pattern = 'k pattern_desc pattern_data

and 'a pattern_data = {
  pat_desc: 'a;
  pat_loc: Location.t;
  pat_extra: (pat_extra * Location.t * attributes) list;
  pat_type: Types.type_expr;
  pat_env: Env.t;
  pat_attributes: attributes;
}

and pat_extra =
  | Tpat_constraint of { type_: core_type }
      (** [_ : t] *)
  | Tpat_type of { path: Path.t; longid: Longident.t Asttypes.loc }
      (** [#tconst] => [{ path = tconst; longid = "tconst" }]

          The corresponding pattern's description is a [Tpat_or _]
          representing the branches of [tconst].
      *)
  | Tpat_open of { path: Path.t; longid: Longident.t Asttypes.loc; env: Env.t }
      (** [M.(...)] => [{ path = M; longid = "M" }] *)
  | Tpat_unpack
      (** [(module ...)] *)

and 'k pattern_desc =
  (* value patterns *)
  | Tpat_any : value pattern_desc
      (** [_] *)
  | Tpat_var : { id: Ident.t; name: string Asttypes.loc } -> value pattern_desc
      (** [x]
          [(module M)] only allowed in [let ... in] (See {!pat_extra.Tpat_unpack})
      *)
  | Tpat_alias :
      { pat: value general_pattern; id: Ident.t; name: string Asttypes.loc }
      -> value pattern_desc
      (** [P as a] *)
  | Tpat_constant : { const: Asttypes.constant } -> value pattern_desc
      (** [1], ['a'], ["string"], [1.0], [1l], [1L], [1n]
          /!\ [true] and [false] are constructs, like [()]
      *)
  | Tpat_tuple : { fields: value general_pattern list } -> value pattern_desc
      (** [(P1, ..., Pn)]

          Invariant: n >= 2
      *)
  | Tpat_construct :
      { longid: Longident.t Asttypes.loc;
        ctor_desc: Types.constructor_description;
        fields: value general_pattern list;
        typing: (Ident.t Asttypes.loc list * core_type) option;
          (** [(existentials * t) option] *)
      }
      -> value pattern_desc
      (** [C]                            => [{ fields = []; typing = None }]
          [C P]                          => [{ fields = [P] }]
          [C (P1, ..., Pn)]              => [{ fields = [P1; ...; Pn] }]
          [C (type a b) (P : t)]           => [{ typing = Some ([a; b], t) }]
          [C (type a b) (P1, ..., Pn : t)] => [{ typing = Some ([a; b], t) }]
      *)
  | Tpat_variant :
      { label: Asttypes.label;
        pat: value general_pattern option;
        row_desc: Types.row_desc ref;
          (** For more information, see
              {{: https://github.com/ocaml/ocaml/blob/4.14.4/typing/types.mli#L273}row_desc constructors and accessors}
          *)
      }
      -> value pattern_desc
      (** [`A]    => [{ pat = None }]
          [`A P]  => [{ pat = Some P }]
      *)
  | Tpat_record :
      { fields:
          ( Longident.t Asttypes.loc
          * Types.label_description
          * value general_pattern
          ) list;
        closed: Asttypes.closed_flag
      }
      -> value pattern_desc
      (** [{ l1=P1; ...; ln=Pn}]      => [{closed = Closed }]
          [{ l1=P1; ...; ln=Pn; _}]   => [(closed = Open }]

          Invariant: n > 0
      *)
  | Tpat_array : { cells: value general_pattern list } -> value pattern_desc
      (** [[|P1; ...; Pn|]] *)
  | Tpat_lazy : { pat: value general_pattern } -> value pattern_desc
      (** [lazy P] *)
  (* computation patterns *)
  | Tpat_value : { pat: tpat_value_argument } -> computation pattern_desc
      (** [P]

          Used in match expr only.

          Invariant: Tpat_value pattern should not carry
          pat_attributes or pat_extra metadata coming from user
          syntax, which must be on the inner pattern node -- to
          facilitate searching for a certain value pattern
          constructor with a specific attributed.
      *)
  | Tpat_exception : { pat: value general_pattern } -> computation pattern_desc
        (** [exception P] *)
  (* generic constructions *)
  | Tpat_or :
      { left_pat: 'k general_pattern;
        right_pat: 'k general_pattern;
        row_desc: Types.row_desc option;
      }
      -> 'k pattern_desc
        (** [P1 | P2] => [{ left_pat = P1; right_pat = P2 }]

            [row_desc] =
            - [Some _] for [#t] (See {!pat_extra.Tpat_type})
            - [None] otherwise.
         *)

and tpat_value_argument = value general_pattern

and expression = {
  exp_desc: expression_desc;
  exp_loc: Location.t;
  exp_extra: (exp_extra * Location.t * attributes) list;
  exp_type: Types.type_expr;
  exp_env: Env.t;
  exp_attributes: attributes;
 }

and exp_extra =
  | Texp_constraint of { type_: core_type }
      (** [... : T] *)
  | Texp_coerce of { from_type: core_type option; to_type: core_type }
      (** [... :> T]      => [{ from_type = None; to_type = T }]
          [... : T0 :> T] => [{ from_type = Some T0; to_type = T }]
      *)
  | Texp_poly of { type_: core_type option }
      (** Used for method bodies. *)
  | Texp_newtype of { name: string }
      (** [fun (type t) -> ...] *)

and expression_desc =
  | Texp_ident of {
        path: Path.t;
        longid: Longident.t Asttypes.loc;
        value_desc: Types.value_description;
      }
      (** [x], [M.x] *)
  | Texp_constant of { const: Asttypes.constant }
      (** [1], ['a'], ["true"], [1.0], [1l], [1L], [1n] *)
  | Texp_let of {
        rec_: Asttypes.rec_flag;
        bindings: value_binding list;
        in_: expression;
      }
      (** [let P1 = E1 and ... and Pn = EN in E]
          =>
            {[
              { rec_ = Nonrecursive;
                bindings =
                  [ { vb_pat = P1; vb_expr = E1 }
                    ...;
                    { vb_pat = Pn; vb_expr = En };
                  ];
                in_ = E;
              }
            ]}

          [let rec ... and ...] => [{ rec_ = Recursive }]
      *)
  | Texp_function of {
        arg_label: Asttypes.arg_label;
        param: Ident.t;
        cases: value case list;
        partial: partial;
      }
      (** [fun P -> E]    => [{ arg_label = Nolabel; cases = [P -> E] }]

          [fun ~l:P -> E] => [{ arg_label = Labelled "l" }]
          [fun ?l:P -> E] => [{ arg_label = Optional "l" }]

          [function P1 -> E1 | ... | Pn -> En ]
          => [{ cases = [P1 -> E1; ...; Pn -> En] }]
      *)
  | Texp_apply of
      { f: expression; args: (Asttypes.arg_label * expression option) list }
      (** [Ef E1 ... En]
          => [{ f = Ef; args = [(Nolabel, Some E1); ...; (Nolabel, Some En)] }]

          Among the arguments:
          - [~l:El] => [(Labelled "l", Some El)]
          - [?o:Eo] => [(Optional "o", Some Eo)]
          - [~o:Eo] ~> [?o:(Some Eo)]
          - Implicitly discarded [?o] ~> [?o:None]
            => [(Optional "o", Some (Texp_construct "None"))]

          In the case of a partial application of a function with
          optional or labeled arguments, some arguments (of any kind:
          positional, labeled, or optional) may be "skipped". I.e., they
          are not provided a value although they appear ahead of arguments
          with values in the function's type.
          Those skipped arguments are translated to
          [(Asttypes.arg_label, None)].
          E.g.
          {[
            let f x ~y z = x + y + z in
            f ~y:3 (* partial application, [x] is skipped, [z] is not *)
          ]}
          => [{ args = [(Nolabel, None); (Labelled "y", Some _)] }]
      *)
  | Texp_match of
      { expr: expression; cases: computation case list; partial: partial }
      (** {[
            match E0 with
            | P1 -> E1
            | P2 | exception P3 -> E2
            | exception P4 -> E3
          ]}
          =>
            {[
              { expr = E0;
                cases =
                  [ P1 -> E1;
                    P2 | exception P3 -> E2;
                    exception P4 -> E3
                  ];
              }
            ]}
      *)
  | Texp_try of { expr: expression; cases: value case list }
      (** [try E with P1 -> E1 | ... | PN -> EN] *)
  | Texp_tuple of { fields: expression list }
      (** [E1, ..., EN] *)
  | Texp_construct of {
        longid: Longident.t Asttypes.loc;
        ctor_desc: Types.constructor_description;
        fields: expression list;
      }
      (** [C]               => [{ fields = [] }]
          [C E]             => [{ fields = [E] }]
          [C (E1, ..., En)] => [{ fields = [E1; ...; En] }]
     *)
  | Texp_variant of { label: Asttypes.label; expr: expression option }
      (** [`X]    => [{ Asttypes.label = "X"; expr = None }]
          [`Y E]  => [{ Asttypes.label = "Y"; expr = Some E }]
      *)
  | Texp_record of {
        fields: (Types.label_description * record_label_definition) array;
        representation: Types.record_representation;
        extended_expression: expression option;
      }
      (** [{ l1=P1; ...; ln=Pn}]         => [{ extended_expression = None }]
          [{ E0 with l1=P1; ...; ln=Pn}] => [{ extended_expression = Some E0 }]

          Invariant: n > 0

          If the type is [{ l1: t1; l2: t2 }], then
          [{ E0 with t2=P2 }]
          =>
            {[
              { fields = [|(l1, Kept t1); (l2, Overriden P2)|];
                extended_expression = Some E0;
              }
            ]}
      *)
  | Texp_field of {
        record: expression;
        longid: Longident.t Asttypes.loc;
        desc: Types.label_description;
      }
      (** [E.f] *)
  | Texp_setfield of {
        record: expression;
        longid: Longident.t Asttypes.loc;
        desc: Types.label_description;
        expr: expression;
      }
      (** [Er.f <- Ev] => [{ record = Er; expr = Ev }] *)
  | Texp_array of { cells: expression list }
      (** [[|E1; ...; En|]] *)
  | Texp_ifthenelse of
      { cond: expression; then_: expression; else_: expression option }
      (** [if Ec then Et] => [{ cond = Ec; then_ = Et; else_ = None }]
          [... else Ee]   => [{ else_ = Some Ee }]
      *)
  | Texp_sequence of { expr1: expression; expr2: expression }
      (** [E1; E2] *)
  | Texp_while of { cond: expression; body: expression }
      (** [while Ec do Eb done] *)
  | Texp_for of {
        counter_id: Ident.t;
        counter_pat: Parsetree.pattern;
        start: expression;
        finish: expression;
        direction: Asttypes.direction_flag;
        body: expression;
      }
      (** [for counter = Es to Ef do Eb done]
          =>
            {[
              { counter_id = "counter";
                counter_pat = counter;
                start = Es;
                finish = Ef;
                direction = Upto;
                body = Eb;
              }
            ]}

          [for ... downto do ...] => [{ direction = Downto }]
      *)
  | Texp_send of { obj: expression; meth: meth }
      (** [Eo#m] *)
  | Texp_new of {
        path: Path.t;
        longid: Longident.t Asttypes.loc;
        class_decl: Types.class_declaration;
      }
      (** [new c] *)
  | Texp_instvar of {
        class_path: Path.t;
        var_path: Path.t;
        name: string Asttypes.loc
      }
      (** [x]. Used for method bodies *)
  | Texp_setinstvar of {
        class_path: Path.t;
        var_path: Path.t;
        name: string Asttypes.loc;
        expr: expression;
      }
      (** [x <- E]. Used for method bodies *)
  | Texp_override of {
        class_path: Path.t;
        instvar_changes: (Ident.t * string Asttypes.loc * expression) list;
      }
      (** [{<var1 = E1; ...; varn = En>}] *)
  | Texp_letmodule of {
        id: Ident.t option;
        name: string option Asttypes.loc;
        presence: Types.module_presence;
        mod_expr: module_expr;
        in_: expression;
      }
      (** [let module M = ME in E] *)
  | Texp_letexception of
      { extension_ctor: extension_constructor; in_: expression }
      (** [let exception C in E] *)
  | Texp_assert of { expr: expression }
      (** [assert E] *)
  | Texp_lazy of { expr: expression }
      (** [lazy E] *)
  | Texp_object of {class_strc: class_structure; meths: string list }
      (** [object ... end] *)
  | Texp_pack of { mod_expr: module_expr }
      (** [(module ME)] *)
  | Texp_letop of {
        let_: binding_op;
        ands: binding_op list;
        param: Ident.t;
        body: value case;
        partial: partial;
      }
      (** [let* P1 = E1 and+ P2 = E2 ... and* Pn = En in E]
          =>
            {[
              { let_ = { "let*"; exp = E1 }
                ands = [{ "and+"; exp = E2 }; ...; { "and*"; exp = En }];
                body = (P1, P2, ..., Pn) -> E;
              }
            ]}
      *)
  | Texp_unreachable
      (** [.] *)
  | Texp_extension_constructor of
      { longid: Longident.t Asttypes.loc; path: Path.t }
      (** [[%id]] *)
  | Texp_open of { open_decl: open_declaration; in_: expression }
      (** [let open M in E] *)

and meth =
  | Tmeth_name of { name: string }
  | Tmeth_val of { id: Ident.t }
  | Tmeth_ancestor of { id: Ident.t; path: Path.t }

and 'k case = {
  c_lhs: 'k general_pattern;
  c_guard: expression option;
  c_rhs: expression;
}

and record_label_definition =
  | Kept of { type_expr: Types.type_expr }
  | Overridden of { longid: Longident.t Asttypes.loc; expr: expression }

and binding_op = {
  bop_op_path: Path.t;
  bop_op_name: string Asttypes.loc;
  bop_op_val: Types.value_description;
  bop_op_type: Types.type_expr;
    (* This is the type at which the operator was used.
       It is always an instance of [bop_op_val.val_type] *)
  bop_exp: expression;
  bop_loc: Location.t;
}

(* Value expressions for the class language *)

and class_expr = {
  cl_desc: class_expr_desc;
  cl_loc: Location.t;
  cl_type: Types.class_type;
  cl_env: Env.t;
  cl_attributes: attributes;
}

and class_expr_desc =
  | Tcl_ident of
      { path: Path.t; longid: Longident.t Asttypes.loc; params: core_type list }
      (** [c]               => [{ path = c; longid = "c";, params = [] }]
          [[t] c]           => [{ params = [t] }]
          [[t1, ..., tn] c] => [{ params = [t1; ...; tn] }]

          This is always wrapped in a [Tcl_constraint].
      *)
  | Tcl_structure of { strc: class_structure }
      (** [object ... end] *)
  | Tcl_fun of {
        arg_label: Asttypes.arg_label;
        arg_pattern: pattern;
        arg_pattern_vars: (Ident.t * expression) list;
          (** maps all pattern variables to idents for use inside methods *)
        body: class_expr;
        partial: partial;
      }
      (** [class c P = CE] *)
  | Tcl_apply of
      { c: class_expr; args: (Asttypes.arg_label * expression option) list }
      (** [c E1 ... En]. Similar to {!Texp_apply} *)
  | Tcl_let of {
        rec_: Asttypes.rec_flag;
        bindings: value_binding list;
        vars: (Ident.t * expression) list;
          (** see {!Tcl_fun.arg_pattern_vars} *)
        class_expr: class_expr;
      }
      (** [let P1 = E1 and ... and Pn = EN in CE]. Similar to {!Texp_let} *)
  | Tcl_constraint of {
        class_expr: class_expr;
        class_type: class_type option;
        instvars: string list (** visible instance variables *);
        meths: string list (** visible methods *);
        concrete_meths: Types.MethSet.t (** concrete methods *);
      }
      (** [(CE : CT)]

          If [class_type = None] then [class_expr.cl_desc = Tcl_ident]
      *)
  | Tcl_open of { open_desc: open_description; in_: class_expr }
      (** [let open M in CE] *)

and class_structure = {
  cstr_self: pattern;
  cstr_fields: class_field list;
  cstr_type: Types.class_signature;
  cstr_meths: Ident.t Types.Meths.t;
}

and class_field = {
  cf_desc: class_field_desc;
  cf_loc: Location.t;
  cf_attributes: attributes;
}

and class_field_kind =
  | Tcfk_virtual of { type_: core_type }
  | Tcfk_concrete of { over: Asttypes.override_flag; expr: expression }

and class_field_desc =
  | Tcf_inherit of {
        over: Asttypes.override_flag;
        parent_class: class_expr;
        parent_alias: string option;
        instvars: (string * Ident.t) list (** inherited instance variables *);
        meths: (string * Ident.t) list (** inherited concrete methods *);
      }
      (** [inherit p]
          => [{ over = Fresh; parent_class = p; parent_alias = None }]

          [inherit! p]      => [{ over = Override }]
          [inherit p as a]  => [{ parent_alias = Some "a" }]
      *)
  | Tcf_val of {
        name: string Asttypes.loc;
        mut: Asttypes.mutable_flag;
        id: Ident.t;
        virt: class_field_kind;
        already_declared: bool;
      }
      (** [val v = E]
          =>
            {[
              { name = "v";
                mut = Immutable;
                virt = Tcfk_concrete { over = Fresh; expr = E };
              }
            ]}

          [val! ...]            => [{ virt = Tcfk_concrete { over = Override }}]
          [val virtual ... : t] => [{ virt = Tcfk_virtual { type_ = t }}]
          [val mutable ...]     => [{ mut = Mutable }]

          TODO: confirm [already_declared] can be true although the value
                is not flagged as overriding
      *)
  | Tcf_method of {
        name: string Asttypes.loc;
        priv: Asttypes.private_flag;
        virt: class_field_kind;
      }
      (** [method m = E]
          =>
            {[
              { name = "m";
                priv = Public;
                virt = Tcfk_concrete { over = Fresh; expr = E };
              }
            ]}

          [method! ... = E]
          => [{ virt = Tcfk_concrete { over = Override; expr = E }}]

          [method virtual ... : t]
          => [{ virt = Tcfk_virtual { type_ = t } }]

          [method private ...] => [{ priv = Private }]
      *)
  | Tcf_constraint of { type1: core_type; type2: core_type }
      (** [contraint t1 = t2] *)
  | Tcf_initializer of { expr: expression }
      (** [initializer E] *)
  | Tcf_attribute of { attribute: attribute }
      (** [[@@@...]] *)

(* Value expressions for the module language *)

and module_expr = {
    mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t;
    mod_attributes: attributes;
   }

(** Annotations for [Tmod_constraint]. *)
and module_type_constraint =
  | Tmodtype_implicit
      (** The module type constraint has been synthesized during
          typechecking.
      *)
  | Tmodtype_explicit of { mod_type: module_type }
      (** The module type was in the source file. *)

and functor_parameter =
  | Unit
      (** [()] *)
  | Named of
      { id: Ident.t option;
        name: string option Asttypes.loc;
        mod_type: module_type;
      }
      (** [(M : MT)] => [{ id = Some M; name = Some "M"; mod_type = MT }]
          [(_ : MT)] => [{ id = None; name = None; mod_type = MT }]
      *)

and module_expr_desc =
  | Tmod_ident of { path: Path.t; longid: Longident.t Asttypes.loc }
      (** [M] *)
  | Tmod_structure of { strc: structure }
      (** [struct ... end] *)
  | Tmod_functor of { param: functor_parameter; body: module_expr }
      (** [...(FP) = MEb], [functor (FP) -> MEb] *)
  | Tmod_apply of
      { ftor: module_expr; arg: module_expr; res_coercion: module_coercion }
      (** [Mf(Ma)] *)
  | Tmod_constraint of {
        mod_expr: module_expr;
        mod_type: Types.module_type;
        constraint_: module_type_constraint;
        coercion: module_coercion;
      }
      (** [ME]        =>  [{ constraint_ = Tmodtype_implicit }]
          [(ME : MT)] =>  [{ constraint_ = Tmodtype_explicit {mod_type = MT} }]
      *)
  | Tmod_unpack of { expr: expression; mod_type: Types.module_type }
      (** (val E) *)

and structure = {
  str_items: structure_item list;
  str_type: Types.signature;
  str_final_env: Env.t;
}

and structure_item = {
  str_desc: structure_item_desc;
  str_loc: Location.t;
  str_env: Env.t;
}

and structure_item_desc =
  | Tstr_eval of { expr: expression; attributes: attributes }
      (** [E] *)
  | Tstr_value of { rec_: Asttypes.rec_flag; bindings: value_binding list }
      (** [let VB1 and ... and VBn]
          => [{ rec_ = Nonecursive; bindings = [VB1; ...; VBn] }]

          [let rec ...] => [{ rec_ = Recursive }]
      *)
  | Tstr_primitive of { val_desc: value_description }
      (** [external VD] *)
  | Tstr_type of { rec_: Asttypes.rec_flag; type_decls: type_declaration list }
      (** [type TD1 and ... and TDn]
          => [{ rec_ = Recursive; type_decls = [TD1; ...; TDn] }]

          [type nonrec ...] => [{ rec_ = Nonrecursive }]
      *)
  | Tstr_typext of { type_ext: type_extension }
      (** [type t += C1 | ... | Cn]
          => [{type_ext = {tyext_path = t; tyext_constructors = [C1; ...; Cn]}}]
      *)
  | Tstr_exception of { type_exc: type_exception }
      (** [exception ...] *)
  | Tstr_module of { mod_binding: module_binding }
      (** [module ... = ...] *)
  | Tstr_recmodule of { mod_bindings: module_binding list }
      (** [module rec ... = ... and ...] *)
  | Tstr_modtype of { modtyp_decl: module_type_declaration }
      (** [module type ... = ...] *)
  | Tstr_open of { open_decl: open_declaration }
      (** [open ...] *)
  | Tstr_class of
      { class_bindings: (class_declaration * string list) list }
      (** [class ... = ... and ...]

          The [string list]s are the methods.
      *)
  | Tstr_class_type of
      { class_types:
          (Ident.t * string Asttypes.loc * class_type_declaration) list
      }
      (** [class type ... = ... and ...] *)
  | Tstr_include of { incl_decl: include_declaration }
      (** [include ...] *)
  | Tstr_attribute of { attribute: attribute }
      (** [[@@@...]] *)

and module_binding = {
  mb_id: Ident.t option;
  mb_name: string option Asttypes.loc;
  mb_presence: Types.module_presence;
  mb_expr: module_expr;
  mb_attributes: attributes;
  mb_loc: Location.t;
}

and value_binding = {
  vb_pat: pattern;
  vb_expr: expression;
  vb_attributes: attributes;
  vb_loc: Location.t;
}

and module_coercion =
  | Tcoerce_none
      (** Coerced [ME] already has the same shape (same items in the same order)
          as the resulting [MT]. Type changes (monomorphization,
          abstraction, ...) are ignored. Only names and positions matter.
          E.g. [module _ : MT = ME] when [module type of ME ~= MT]
      *)
  | Tcoerce_structure of {
      pos_coercions: (int * module_coercion) list;
      id_pos_list: (Ident.t * int * module_coercion) list;
    }
      (** Coerced [ME] contains more items or in a different order than the
          resulting [MT]. Shadowed items in [ME] are accounted for.
          E.g. [module _ : MT = ME] when [MT ⊂ module type of ME]
      *)
  | Tcoerce_functor of
      { arg_coercion: module_coercion; res_coercion: module_coercion }
      (** At least one of the argument or the result is acutally coerced.
          The argument's coercion is reversed. I.e., the source [MT] expects
          more items or in a different order than the destination [MT].
          E.g.
            {[
            module F : functor (P : sig val x : int end) -> sig end =
              functor (P : sig end) -> struct let x = 0 end
            ]}
      *)
  | Tcoerce_primitive of { coercion: primitive_coercion }
      (** A primitive value ([external ...]) in the source [MT] is coerced as a
          non-primitive (typically, a regular value) in the destination [MT].
          This is always wrapped in a [Tcoerce_structure].
      *)
  | Tcoerce_alias of { env: Env.t; path: Path.t; coercion: module_coercion }
      (** A submodule is an alias.
          This is always wrapped in a [Tcoerce_structure].
      *)

and module_type = {
    mty_desc: module_type_desc;
    mty_type: Types.module_type;
    mty_env: Env.t;
    mty_loc: Location.t;
    mty_attributes: attributes;
   }

and module_type_desc =
  | Tmty_ident of { path: Path.t; longid: Longident.t Asttypes.loc }
      (** [S] *)
  | Tmty_signature of { sign: signature }
      (** [sig ... end] *)
  | Tmty_functor of { param: functor_parameter; res_type: module_type }
      (** [functor (FP) -> MT] *)
  | Tmty_with of {
        mod_type: module_type;
        constraints: (Path.t * Longident.t Asttypes.loc * with_constraint) list;
      }
      (** [MT with ...] *)
  | Tmty_typeof of { mod_expr: module_expr }
      (** [module type of ME] *)
  | Tmty_alias of { path: Path.t; longid: Longident.t Asttypes.loc }
      (** [M] *)

and primitive_coercion = {
  pc_desc: Primitive.description;
  pc_type: Types.type_expr;
  pc_env: Env.t;
  pc_loc: Location.t;
}

and signature = {
  sig_items: signature_item list;
  sig_type: Types.signature;
  sig_final_env: Env.t;
}

and signature_item = {
  sig_desc: signature_item_desc;
  sig_env: Env.t;
  sig_loc: Location.t;
}

and signature_item_desc =
  | Tsig_value of { val_desc: value_description }
      (** [val ...] *)
  | Tsig_type of { rec_: Asttypes.rec_flag; type_decls: type_declaration list }
      (** [type ... and ...]. See {!structure_item_desc.Tstr_type}. *)
  | Tsig_typesubst of { type_decls: type_declaration list }
      (** [type ... := ... and ...] *)
  | Tsig_typext of { type_ext: type_extension }
      (** [type ... += ...]. See {!structure_item_desc.Tstr_typext}. *)
  | Tsig_exception of { type_exc: type_exception }
      (** [exception ...] *)
  | Tsig_module of { mod_decl: module_declaration }
      (** [module ... : ...], [module ... = ...] *)
  | Tsig_modsubst of { mod_subst: module_substitution }
      (** [module ... := ...] *)
  | Tsig_recmodule of { mod_delcs: module_declaration list }
      (** [module rec ... : ... and ...] *)
  | Tsig_modtype of { modtype_decl: module_type_declaration }
      (** [module type ...] *)
  | Tsig_modtypesubst of { modtype_decl: module_type_declaration }
      (** [module type ... := ...] *)
  | Tsig_open of { open_desc: open_description }
      (** [open ...] *)
  | Tsig_include of { incl_desc: include_description }
      (** [include ...] *)
  | Tsig_class of { class_descs: class_description list }
      (** [class ... : ... and ...] *)
  | Tsig_class_type of { classtype_decls: class_type_declaration list }
      (** [class type ... = ... and ...] *)
  | Tsig_attribute of { attribute: attribute }
      (** [[@@@...]] *)

and module_declaration = {
  md_id: Ident.t option;
  md_name: string option Asttypes.loc;
  md_presence: Types.module_presence;
  md_type: module_type;
  md_attributes: attributes;
  md_loc: Location.t;
}

and module_substitution = {
  ms_id: Ident.t;
  ms_name: string Asttypes.loc;
  ms_manifest: Path.t;
  ms_txt: Longident.t Asttypes.loc;
  ms_attributes: attributes;
  ms_loc: Location.t;
}

and module_type_declaration = {
  mtd_id: Ident.t;
  mtd_name: string Asttypes.loc;
  mtd_type: module_type option;
  mtd_attributes: attributes;
  mtd_loc: Location.t;
}

and 'a open_infos = {
  open_expr: 'a;
  open_bound_items: Types.signature;
  open_override: Asttypes.override_flag;
  open_env: Env.t;
  open_loc: Location.t;
  open_attributes: attributes;
}

and open_description = (Path.t * Longident.t Asttypes.loc) open_infos

and open_declaration = module_expr open_infos


and 'a include_infos = {
  incl_mod: 'a;
  incl_type: Types.signature;
  incl_loc: Location.t;
  incl_attributes: attributes;
}

and include_description = module_type include_infos

and include_declaration = module_expr include_infos

and with_constraint =
  | Twith_type of { type_decl: type_declaration }
      (** [with type ... = ...] *)
  | Twith_module of { path: Path.t ; longid: Longident.t Asttypes.loc }
      (** [with module ... = ...] *)
  | Twith_modtype of { mod_type: module_type }
      (** [with module type ... = ...] *)
  | Twith_typesubst of { type_decl: type_declaration }
      (** [with type ... := ...] *)
  | Twith_modsubst of { path: Path.t; longid: Longident.t Asttypes.loc }
      (** [with module ... := ...] *)
  | Twith_modtypesubst of { mod_type: module_type }
      (** [with module type ... := ...] *)

and core_type = {
    mutable ctyp_desc: core_type_desc;
      (** mutable because of [Typeclass.declare_method] *)
    mutable ctyp_type: Types.type_expr;
      (** mutable because of [Typeclass.declare_method] *)
    ctyp_env: Env.t;
    ctyp_loc: Location.t;
    ctyp_attributes: attributes;
   }

and core_type_desc =
  | Ttyp_any
      (** [_] *)
  | Ttyp_var of {name: string }
      (** ['a] *)
  | Ttyp_arrow of
      { arg_label: Asttypes.arg_label;
        arg_type: core_type;
        res_type: core_type;
      }
      (** [t1 -> t2], [~l:t1 -> t2], [?o:t1 -> t2] *)
  | Ttyp_tuple of { fields: core_type list }
      (** [(t1 * ... * t2)] *)
  | Ttyp_constr of
      { path: Path.t; longid: Longident.t Asttypes.loc; params: core_type list }
      (** [(t1, ..., tn) t] *)
  | Ttyp_object of { fields: object_field list; closed: Asttypes.closed_flag }
      (** [<m1: t1; ...; mn: tn>]     => [{ closed = Closed }]
          [<m1: t1; ...; mn: tn; ..>] => [{ closed = Open }]
      *)
  | Ttyp_class of
      { path: Path.t; longid: Longident.t Asttypes.loc; params: core_type list }
      (** [(t1, ..., tn) #t] *)
  | Ttyp_alias of { type_: core_type; name: string }
      (** [t as name] *)
  | Ttyp_variant of {
        rows: row_field list;
        closed: Asttypes.closed_flag;
        labels: Asttypes.label list option;
      }
      (** [[`C1 | ... | `Cn]]
          => [{ rows = [C1; ...; Cn]; closed = Closed; labels = None }]

          [[>`C1 | ... | `Cn]]  => [{ closed = Open }]
          [[<`C1 | ... | `Cn]]  => [{ closed = Closed; labels = Some [] }]
          [[<`C1 | ... | `Cn > `Li ... `Lm]]
          =>  [{ closed = Closed; labels = Some [Li; ...; Lm] }]
              where `Li ... `Lm are labels among `C1 .. `Cn
              e.g.
                {[
                  type 'a t = [< `Int of int
                              |  `String of string
                              |  `Float of float
                              > `Float `Int
                              ] as 'a
                ]}
      *)
  | Ttyp_poly of { params: string list; type_: core_type }
      (** ['a1 ... 'an . t] *)
  | Ttyp_package of { pack_type: package_type }
      (** [(module S)] *)

and package_type = {
  pack_path: Path.t;
  pack_fields: (Longident.t Asttypes.loc * core_type) list;
  pack_type: Types.module_type;
  pack_txt: Longident.t Asttypes.loc;
}

and row_field = {
  rf_desc: row_field_desc;
  rf_loc: Location.t;
  rf_attributes: attributes;
}

and row_field_desc =
  | Ttag of { name: string Asttypes.loc; empty: bool; conj: core_type list }
      (** [`V]                    => [{ name = "V"; empty = true; conj = [] }]
          [`V of t]               => [{ empty = false; conj = [t] }]
          [`V of t1 & ... & tn]   => [{ empty = false; conj = [t1; ...; tn] }]
          [`V of & t1 & ... & tn] => [{ empty = true; conj = [t1; ...; tn] }]
      *)
  | Tinherit of { type_: core_type }
      (** [[ | t]]*)

and object_field = {
  of_desc: object_field_desc;
  of_loc: Location.t;
  of_attributes: attributes;
}

and object_field_desc =
  | OTtag of { name: string Asttypes.loc; type_: core_type }
      (** [<m:t>] *)
  | OTinherit of { type_: core_type }
      (** [<t>] *)

and value_description = {
  val_id: Ident.t;
  val_name: string Asttypes.loc;
  val_desc: core_type;
  val_val: Types.value_description;
  val_prim: string list;
  val_loc: Location.t;
  val_attributes: attributes;
}

and type_declaration = {
  typ_id: Ident.t;
  typ_name: string Asttypes.loc;
  typ_params: (core_type * (Asttypes.variance * Asttypes.injectivity)) list;
  typ_type: Types.type_declaration;
  typ_cstrs: (core_type * core_type * Location.t) list;
  typ_kind: type_kind;
  typ_private: Asttypes.private_flag;
  typ_manifest: core_type option;
  typ_loc: Location.t;
  typ_attributes: attributes;
}

and type_kind =
  | Ttype_abstract
      (** [type t], [type t1 = t2], [type t = <...>] *)
  | Ttype_variant of { ctor_decls: constructor_declaration list }
      (** [type t = | ...] *)
  | Ttype_record of { label_decls: label_declaration list }
      (** [type t = { ... }] *)
  | Ttype_open
      (** [type t = ..] *)

and label_declaration = {
  ld_id: Ident.t;
  ld_name: string Asttypes.loc;
  ld_mutable: Asttypes.mutable_flag;
  ld_type: core_type;
  ld_loc: Location.t;
  ld_attributes: attributes;
}

and constructor_declaration = {
  cd_id: Ident.t;
  cd_name: string Asttypes.loc;
  cd_vars: string Asttypes.loc list;
  cd_args: constructor_arguments;
  cd_res: core_type option;
  cd_loc: Location.t;
  cd_attributes: attributes;
}

and constructor_arguments =
  | Cstr_tuple of { fields: core_type list }
      (** [C], [C of t], [C of t1 * ... * tn] *)
  | Cstr_record of { label_decls: label_declaration list }
      (** [C of { ... }] *)

and type_extension = {
  tyext_path: Path.t;
  tyext_txt: Longident.t Asttypes.loc;
  tyext_params: (core_type * (Asttypes.variance * Asttypes.injectivity)) list;
  tyext_constructors: extension_constructor list;
  tyext_private: Asttypes.private_flag;
  tyext_loc: Location.t;
  tyext_attributes: attributes;
}

and type_exception = {
  tyexn_constructor: extension_constructor;
  tyexn_loc: Location.t;
  tyexn_attributes: attributes;
}

and extension_constructor = {
  ext_id: Ident.t;
  ext_name: string Asttypes.loc;
  ext_type: Types.extension_constructor;
  ext_kind: extension_constructor_kind;
  ext_loc: Location.t;
  ext_attributes: attributes;
}

and extension_constructor_kind =
  | Text_decl of {
        existentials: string Asttypes.loc list;
        arg: constructor_arguments;
        res_type: core_type option (** for GADT *);
      }
      (** [C]
          =>
            {[
              { existentials = [];
                arg = Cstr_tuple { fields = [] };
                res_type = None;
              }
            ]}

          [C of ...]              => [arg] changes. See {!constructor_arguments}
          [C : t], [C : ... -> t] => [{ res_type = Some t }]
          [C: 'a1 ... 'an . ...]  => [{ existentials = ["a1"; ...; "an"] }]
      *)
  | Text_rebind of { path: Path.t; longid: Longident.t Asttypes.loc }
      (** [C1 = C2] => [{ path = C2; longid = "C2" }] *)

and class_type = {
  cltyp_desc: class_type_desc;
  cltyp_type: Types.class_type;
  cltyp_env: Env.t;
  cltyp_loc: Location.t;
  cltyp_attributes: attributes;
}

and class_type_desc =
  | Tcty_constr of
      { path: Path.t; longid: Longident.t Asttypes.loc; params: core_type list }
      (** [c], [[t1, ..., tn] c] *)
  | Tcty_signature of { class_sign: class_signature }
      (** [object ... end] *)
  | Tcty_arrow of {
        arg_label: Asttypes.arg_label;
        arg_type: core_type;
        class_type: class_type
      }
      (** [t -> CT], [~l:t -> CT], [?o:t -> CT] *)
  | Tcty_open of { open_desc: open_description; class_type: class_type }
      (** [let open M in CT] *)

and class_signature = {
  csig_self: core_type;
  csig_fields: class_type_field list;
  csig_type: Types.class_signature;
}

and class_type_field = {
  ctf_desc: class_type_field_desc;
  ctf_loc: Location.t;
  ctf_attributes: attributes;
}

and class_type_field_desc =
  | Tctf_inherit of { parent_type: class_type }
      (** [inherit CT] *)
  | Tctf_val of {
        name: string;
        mut: Asttypes.mutable_flag;
        virt: Asttypes.virtual_flag;
        type_: core_type;
      }
      (** [val v : t]. See {!class_field_desc.Tcf_val} *)
  | Tctf_method of {
        name: string;
        priv: Asttypes.private_flag;
        virt: Asttypes.virtual_flag;
        type_: core_type;
      }
      (** [method m : t]. See {!class_field_desc.Tcf_method} *)
  | Tctf_constraint of { type1: core_type; type2: core_type }
      (** [contraint t1 = t2] *)
  | Tctf_attribute of { attribute: attribute }
      (** [[@@@...]] *)

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and 'a class_infos = {
  ci_virt: Asttypes.virtual_flag;
  ci_params: (core_type * (Asttypes.variance * Asttypes.injectivity)) list;
  ci_id_name: string Asttypes.loc;
  ci_id_class: Ident.t;
  ci_id_class_type: Ident.t;
  ci_id_object: Ident.t;
  ci_id_typehash: Ident.t;
  ci_expr: 'a;
  ci_decl: Types.class_declaration;
  ci_type_decl: Types.class_type_declaration;
  ci_loc: Location.t;
  ci_attributes: attributes;
}
