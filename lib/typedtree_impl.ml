include Typedtree_intf

module OCaml = OCaml.Typedtree

(* [of_*] and [to_*] are symmetrical. Unless specified otherwise, one can
  be trivially derived from the other. *)

(* [of_*] conversion functions *)

let of_partial : OCaml.partial -> partial = function
  | Partial -> Partial
  | Total -> Total

let of_attribute : OCaml.attribute -> attribute = Fun.id

let of_attributes : OCaml.attributes -> attributes = Fun.id

let of_value : OCaml.value -> value = Fun.id

let of_computation : OCaml.computation -> computation = Fun.id

let rec of_pattern : OCaml.pattern -> pattern = fun pat ->
  of_general_pattern pat

and of_general_pattern : type k . k OCaml.general_pattern -> k general_pattern = fun pat ->
  of_pattern_data ~of_pat_desc:of_pattern_desc pat

and of_pattern_data : type a b . of_pat_desc:(a -> b) -> a OCaml.pattern_data -> b pattern_data =
  fun ~of_pat_desc pat_data ->
  let pat_desc = of_pat_desc pat_data.pat_desc in
  let pat_loc = pat_data.pat_loc in
  let pat_extra =
    let convert_of (px, loc, attrs) =
      let px' = of_pat_extra px in
      let attrs' = of_attributes attrs in
      (px', loc, attrs')
    in
    List.map convert_of pat_data.pat_extra
  in
  let pat_type = pat_data.pat_type in
  let pat_env = pat_data.pat_env in
  let pat_attributes = of_attributes pat_data.pat_attributes in
  { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes }

and of_pat_extra : OCaml.pat_extra -> pat_extra = function
  | Tpat_constraint type_ ->
      let type_ = of_core_type type_ in
      Tpat_constraint { type_ }
  | Tpat_type (path, longid) -> Tpat_type { path; longid }
  | Tpat_open (path, longid, env) -> Tpat_open { path; longid; env }
  | Tpat_unpack -> Tpat_unpack

and of_pattern_desc : type k . k OCaml.pattern_desc -> k pattern_desc = function
  (* value patterns *)
  | Tpat_any -> Tpat_any
  | Tpat_var (id, name) -> Tpat_var { id; name }
  | Tpat_alias (pat, id, name) ->
      let pat = of_general_pattern pat in
      Tpat_alias { pat; id; name }
  | Tpat_constant const -> Tpat_constant { const }
  | Tpat_tuple fields ->
      let fields = List.map of_general_pattern fields in
      Tpat_tuple { fields }
  | Tpat_construct (longid, ctor_desc, fields, typing) ->
      let fields = List.map of_general_pattern fields in
      let typing =
        let convert (ids, t) =
          let t' = of_core_type t in
          (ids, t')
        in
        Option.map convert typing
      in
      Tpat_construct { longid; ctor_desc; fields; typing }
  | Tpat_variant (label, pat, row_desc) ->
      let pat = Option.map of_general_pattern pat in
      (* XXX: reusing the same row_desc, which is a ref *)
      Tpat_variant { label; pat; row_desc }
  | Tpat_record (fields, closed) ->
      let fields =
        let convert (id, lab, pat) =
          let pat' = of_general_pattern pat in
          (id, lab, pat')
        in
        List.map convert fields
      in
      Tpat_record { fields; closed }
  | Tpat_array cells ->
      let cells = List.map of_general_pattern cells in
      Tpat_array { cells }
  | Tpat_lazy pat ->
      let pat = of_general_pattern pat in
      Tpat_lazy { pat }
  (* computation patterns *)
  | Tpat_value pat ->
      let pat = of_tpat_value_argument pat in
      Tpat_value { pat }
  | Tpat_exception pat ->
      let pat = of_general_pattern pat in
      Tpat_exception { pat }
  (* generic constructions *)
  | Tpat_or (left_pat, right_pat, row_desc) ->
      let left_pat = of_general_pattern left_pat in
      let right_pat = of_general_pattern right_pat in
      Tpat_or { left_pat; right_pat; row_desc }

and of_tpat_value_argument : OCaml.tpat_value_argument -> tpat_value_argument =
  fun pat ->
  of_general_pattern (pat :> OCaml.value OCaml.general_pattern)

and of_expression : OCaml.expression -> expression = fun expr ->
  let exp_desc = of_expression_desc expr.exp_desc in
  let exp_loc = expr.exp_loc in
  let exp_extra =
    let convert_of (ex, loc, attrs) =
      let ex' = of_exp_extra ex in
      let attrs' = of_attributes attrs in
      (ex', loc, attrs')
    in
    List.map convert_of expr.exp_extra
  in
  let exp_type = expr.exp_type in
  let exp_env = expr.exp_env in
  let exp_attributes = of_attributes expr.exp_attributes in
  { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes }

and of_exp_extra : OCaml.exp_extra -> exp_extra = function
  | Texp_constraint (type_) ->
      let type_ = of_core_type type_ in
      Texp_constraint { type_ }
  | Texp_coerce (from_type, to_type) ->
      let from_type = Option.map of_core_type from_type in
      let to_type = of_core_type to_type in
      Texp_coerce { from_type; to_type }
  | Texp_poly (type_) ->
      let type_ = Option.map of_core_type type_ in
      Texp_poly { type_ }
  | Texp_newtype (name) -> Texp_newtype { name }

and of_expression_desc : OCaml.expression_desc -> expression_desc = function
  | Texp_ident (path, longid, value_desc) ->
      Texp_ident { path; longid; value_desc }
  | Texp_constant const -> Texp_constant { const }
  | Texp_let (rec_, bindings, in_) ->
      let bindings = List.map of_value_binding bindings in
      let in_ = of_expression in_ in
      Texp_let { rec_; bindings; in_ }
  | Texp_function { arg_label; param; cases; partial } ->
      let cases = List.map of_case cases in
      let partial = of_partial partial in
      Texp_function { arg_label; param; cases; partial }
  | Texp_apply (f, args) ->
      let f = of_expression f in
      let args =
        let convert (lab, expr) =
          let expr = Option.map of_expression expr in
          (lab, expr)
        in
        List.map convert args
      in
      Texp_apply { f; args }
  | Texp_match (expr, cases, partial) ->
      let expr = of_expression expr in
      let cases = List.map of_case cases in
      let partial = of_partial partial in
      Texp_match { expr; cases; partial }
  | Texp_try (expr, cases) ->
      let expr = of_expression expr in
      let cases = List.map of_case cases in
      Texp_try { expr; cases }
  | Texp_tuple fields ->
      let fields = List.map of_expression fields in
      Texp_tuple { fields }
  | Texp_construct (longid, ctor_desc, fields) ->
      let fields = List.map of_expression fields in
      Texp_construct { longid; ctor_desc; fields }
  | Texp_variant (label, expr) ->
      let expr = Option.map of_expression expr in
      Texp_variant { label; expr }
  | Texp_record { fields; representation; extended_expression } ->
      let fields =
        let convert (lab, rld) =
          let rld' = of_record_label_definition rld in
          (lab, rld')
        in
        Array.map convert fields
      in
      let extended_expression = Option.map of_expression extended_expression in
      Texp_record { fields; representation; extended_expression }
  | Texp_field (record, longid, desc) ->
      let record = of_expression record in
      Texp_field { record; longid; desc }
  | Texp_setfield (record, longid, desc, expr) ->
      let record = of_expression record in
      let expr = of_expression expr in
      Texp_setfield { record; longid; desc; expr }
  | Texp_array cells ->
      let cells = List.map of_expression cells in
      Texp_array { cells }
  | Texp_ifthenelse (cond, then_, else_) ->
      let cond = of_expression cond in
      let then_ = of_expression then_ in
      let else_ = Option.map of_expression else_ in
      Texp_ifthenelse { cond; then_; else_}
  | Texp_sequence (expr1, expr2) ->
      let expr1 = of_expression expr1 in
      let expr2 = of_expression expr2 in
      Texp_sequence { expr1; expr2 }
  | Texp_while (cond, body) ->
      let cond = of_expression cond in
      let body = of_expression body in
      Texp_while { cond; body }
  | Texp_for (counter_id, counter_pat, start, finish, direction, body) ->
      let start = of_expression start in
      let finish = of_expression finish in
      let body = of_expression body in
      Texp_for { counter_id; counter_pat; start; finish; direction; body }
  | Texp_send (obj, meth) ->
      let obj = of_expression obj in
      let meth = of_meth meth in
      Texp_send { obj; meth }
  | Texp_new (path, longid, class_decl) -> Texp_new { path; longid; class_decl }
  | Texp_instvar (class_path, var_path, name) ->
      Texp_instvar { class_path; var_path; name }
  | Texp_setinstvar (class_path, var_path, name, expr) ->
      let expr = of_expression expr in
      Texp_setinstvar { class_path; var_path; name; expr }
  | Texp_override (class_path, instvar_changes) ->
      let instvar_changes =
        let convert (id, name, expr) =
          let expr' = of_expression expr in
          (id, name, expr')
        in
        List.map convert instvar_changes
      in
      Texp_override { class_path; instvar_changes }
  | Texp_letmodule (id, name, presence, mod_expr, in_) ->
      let mod_expr = of_module_expr mod_expr in
      let in_ = of_expression in_ in
      Texp_letmodule { id; name; presence; mod_expr; in_ }
  | Texp_letexception (extension_ctor, in_) ->
      let extension_ctor = of_extension_constructor extension_ctor in
      let in_ = of_expression in_ in
      Texp_letexception { extension_ctor; in_ }
  | Texp_assert expr ->
      let expr = of_expression expr in
      Texp_assert { expr }
  | Texp_lazy expr ->
      let expr = of_expression expr in
      Texp_lazy { expr }
  | Texp_object (class_strc, meths) ->
      let class_strc = of_class_structure class_strc in
      Texp_object { class_strc; meths }
  | Texp_pack mod_expr ->
      let mod_expr = of_module_expr mod_expr in
      Texp_pack { mod_expr }
  | Texp_letop { let_; ands; param; body; partial } ->
      let let_ = of_binding_op let_ in
      let ands = List.map of_binding_op ands in
      let body = of_case body in
      let partial = of_partial partial in
      Texp_letop { let_; ands; param; body; partial }
  | Texp_unreachable -> Texp_unreachable
  | Texp_extension_constructor (longid, path) ->
      Texp_extension_constructor { longid; path }
  | Texp_open (open_decl, in_) ->
      let open_decl = of_open_declaration open_decl in
      let in_ = of_expression in_ in
      Texp_open { open_decl; in_ }

and of_meth : OCaml.meth -> meth = function
  | Tmeth_name name -> Tmeth_name { name }
  | Tmeth_val id -> Tmeth_val { id }
  | Tmeth_ancestor (id, path) -> Tmeth_ancestor { id; path }

and of_case : 'k . 'k OCaml.case -> 'k case = fun case ->
  let c_lhs = of_general_pattern case.c_lhs in
  let c_guard = Option.map of_expression case.c_guard in
  let c_rhs = of_expression case.c_rhs in
  { c_lhs; c_guard; c_rhs }

and of_record_label_definition :
  OCaml.record_label_definition -> record_label_definition =
  function
  | Kept type_expr -> Kept { type_expr }
  | Overridden (longid, expr) ->
      let expr = of_expression expr in
      Overridden { longid; expr }

and of_binding_op : OCaml.binding_op -> binding_op = fun bop ->
  let bop_op_path = bop.bop_op_path in
  let bop_op_name = bop.bop_op_name in
  let bop_op_val = bop.bop_op_val in
  let bop_op_type = bop.bop_op_type in
  let bop_exp = of_expression bop.bop_exp in
  let bop_loc = bop.bop_loc in
  { bop_op_path; bop_op_name; bop_op_val; bop_op_type; bop_exp; bop_loc }

and of_class_expr : OCaml.class_expr -> class_expr = fun cl ->
  let cl_desc = of_class_expr_desc cl.cl_desc in
  let cl_loc = cl.cl_loc in
  let cl_type = cl.cl_type in
  let cl_env = cl.cl_env in
  let cl_attributes = of_attributes cl.cl_attributes in
  { cl_desc; cl_loc; cl_type; cl_env; cl_attributes }

and of_class_expr_desc : OCaml.class_expr_desc -> class_expr_desc = function
  | Tcl_ident (path, longid, params) ->
        let params = List.map of_core_type params in
        Tcl_ident { path; longid; params }
  | Tcl_structure strc ->
      let strc = of_class_structure strc in
      Tcl_structure { strc }
  | Tcl_fun (arg_label, arg_pattern, arg_pattern_vars, body, partial) ->
      let arg_pattern = of_pattern arg_pattern in
      let arg_pattern_vars =
        let convert (id, expr) =
          let expr' = of_expression expr in
          (id, expr')
        in
        List.map convert arg_pattern_vars
      in
      let body = of_class_expr body in
      let partial = of_partial partial in
      Tcl_fun { arg_label; arg_pattern; arg_pattern_vars; body; partial }
  | Tcl_apply (c, args) ->
      let c = of_class_expr c in
      let args =
        let convert (lab, expr) =
          let expr' = Option.map of_expression expr in
          (lab, expr')
        in
        List.map convert args
      in
      Tcl_apply { c; args }
  | Tcl_let (rec_, bindings, vars, class_expr) ->
      let bindings = List.map of_value_binding bindings in
      let vars =
        let convert (id, expr) =
          let expr' = of_expression expr in
          (id, expr')
        in
        List.map convert vars
      in
      let class_expr = of_class_expr class_expr in
      Tcl_let { rec_; bindings; vars; class_expr }
  | Tcl_constraint (class_expr, class_type, instvars, meths, concrete_meths) ->
      let class_expr = of_class_expr class_expr in
      let class_type = Option.map of_class_type class_type in
      Tcl_constraint { class_expr; class_type; instvars; meths; concrete_meths }
  | Tcl_open (open_desc, in_) ->
      let open_desc = of_open_description open_desc in
      let in_ = of_class_expr in_ in
      Tcl_open { open_desc; in_ }

and of_class_structure : OCaml.class_structure -> class_structure = fun cstr ->
  let cstr_self = of_pattern cstr.cstr_self in
  let cstr_fields = List.map of_class_field cstr.cstr_fields in
  let cstr_type = cstr.cstr_type in
  let cstr_meths = cstr.cstr_meths in
  { cstr_self; cstr_fields; cstr_type; cstr_meths }

and of_class_field : OCaml.class_field -> class_field = fun cf ->
  let cf_desc = of_class_field_desc cf.cf_desc in
  let cf_loc = cf.cf_loc in
  let cf_attributes = of_attributes cf.cf_attributes in
  { cf_desc; cf_loc; cf_attributes }

and of_class_field_kind : OCaml.class_field_kind -> class_field_kind = function
  | Tcfk_virtual type_ ->
      let type_ = of_core_type type_ in
      Tcfk_virtual { type_ }
  | Tcfk_concrete (over, expr) ->
      let expr = of_expression expr in
      Tcfk_concrete{ over; expr }

and of_class_field_desc : OCaml.class_field_desc -> class_field_desc = function
  | Tcf_inherit (over, parent_class, parent_alias, instvars, meths) ->
      let parent_class = of_class_expr parent_class in
      Tcf_inherit { over; parent_class; parent_alias; instvars; meths }
  | Tcf_val (name, mut, id, virt, already_declared) ->
      let virt = of_class_field_kind virt in
      Tcf_val { name; mut; id; virt; already_declared }
  | Tcf_method (name, priv, virt) ->
      let virt = of_class_field_kind virt in
      Tcf_method { name; priv; virt }
  | Tcf_constraint (type1, type2) ->
      let type1 = of_core_type type1 in
      let type2 = of_core_type type2 in
      Tcf_constraint { type1; type2 }
  | Tcf_initializer expr ->
      let expr = of_expression expr in
      Tcf_initializer { expr }
  | Tcf_attribute attribute ->
      let attribute = of_attribute attribute in
      Tcf_attribute { attribute }

and of_module_expr : OCaml.module_expr -> module_expr = fun me ->
    let mod_desc = of_module_expr_desc me.mod_desc in
    let mod_loc = me.mod_loc in
    let mod_type = me.mod_type in
    let mod_env = me.mod_env in
    let mod_attributes = of_attributes me.mod_attributes in
    { mod_desc; mod_loc; mod_type; mod_env; mod_attributes }

and of_module_type_constraint:
  OCaml.module_type_constraint -> module_type_constraint =
  function
  | Tmodtype_implicit -> Tmodtype_implicit
  | Tmodtype_explicit mod_type ->
      let mod_type = of_module_type mod_type in
      Tmodtype_explicit { mod_type }

and of_functor_parameter : OCaml.functor_parameter -> functor_parameter = function
  | Unit -> Unit
  | Named (id, name, mod_type) ->
      let mod_type = of_module_type mod_type in
      Named { id; name; mod_type }

and of_module_expr_desc : OCaml.module_expr_desc -> module_expr_desc = function
  | Tmod_ident (path, longid) -> Tmod_ident { path; longid }
  | Tmod_structure strc ->
      let strc = of_structure strc in
      Tmod_structure { strc }
  | Tmod_functor (param, body) ->
      let param = of_functor_parameter param in
      let body = of_module_expr body in
      Tmod_functor { param; body }
  | Tmod_apply (ftor, arg, res_coercion) ->
      let ftor = of_module_expr ftor in
      let arg = of_module_expr arg in
      let res_coercion = of_module_coercion res_coercion in
      Tmod_apply { ftor; arg; res_coercion }
  | Tmod_constraint (mod_expr, mod_type, constraint_, coercion) ->
      let mod_expr = of_module_expr mod_expr in
      let constraint_ = of_module_type_constraint constraint_ in
      let coercion = of_module_coercion coercion in
      Tmod_constraint { mod_expr; mod_type; constraint_; coercion }
  | Tmod_unpack (expr, mod_type) ->
      let expr = of_expression expr in
      Tmod_unpack { expr; mod_type }

and of_structure : OCaml.structure -> structure = fun strc ->
  let str_items = List.map of_structure_item strc.str_items in
  let str_type = strc.str_type in
  let str_final_env = strc.str_final_env in
  { str_items; str_type; str_final_env }

and of_structure_item : OCaml.structure_item -> structure_item = fun strc ->
  let str_desc = of_structure_item_desc strc.str_desc in
  let str_loc = strc.str_loc in
  let str_env = strc.str_env in
  { str_desc; str_loc; str_env }

and of_structure_item_desc : OCaml.structure_item_desc -> structure_item_desc =
  function
  | Tstr_eval (expr, attributes) ->
      let expr = of_expression expr in
      let attributes = of_attributes attributes in
      Tstr_eval { expr; attributes }
  | Tstr_value (rec_, bindings) ->
      let bindings = List.map of_value_binding bindings in
      Tstr_value { rec_; bindings }
  | Tstr_primitive val_desc ->
      let val_desc = of_value_description val_desc in
      Tstr_primitive { val_desc }
  | Tstr_type (rec_, type_decls) ->
      let type_decls = List.map of_type_declaration type_decls in
      Tstr_type { rec_; type_decls }
  | Tstr_typext type_ext ->
      let type_ext = of_type_extension type_ext in
      Tstr_typext { type_ext }
  | Tstr_exception type_exc ->
      let type_exc = of_type_exception type_exc in
      Tstr_exception { type_exc }
  | Tstr_module mod_binding ->
      let mod_binding = of_module_binding mod_binding in
      Tstr_module { mod_binding }
  | Tstr_recmodule mod_bindings ->
      let mod_bindings = List.map of_module_binding mod_bindings in
      Tstr_recmodule { mod_bindings }
  | Tstr_modtype modtyp_decl ->
      let modtyp_decl = of_module_type_declaration modtyp_decl in
      Tstr_modtype { modtyp_decl }
  | Tstr_open open_decl ->
      let open_decl = of_open_declaration open_decl in
      Tstr_open { open_decl }
  | Tstr_class class_bindings ->
      let class_bindings =
        let convert (class_decl, meths) =
          let class_decl' = of_class_declaration class_decl in
          (class_decl', meths)
        in
        List.map convert class_bindings
      in
      Tstr_class { class_bindings }
  | Tstr_class_type class_types ->
      let class_types =
        let convert (id, name, clty_decl) =
          let clty_decl' = of_class_type_declaration clty_decl in
          (id, name, clty_decl')
        in
        List.map convert class_types
      in
      Tstr_class_type { class_types }
  | Tstr_include incl_decl ->
      let incl_decl = of_include_declaration incl_decl in
      Tstr_include { incl_decl }
  | Tstr_attribute attribute ->
      let attribute = of_attribute attribute in
      Tstr_attribute { attribute }

and of_module_binding : OCaml.module_binding -> module_binding = fun mb ->
  let mb_id = mb.mb_id in
  let mb_name = mb.mb_name in
  let mb_presence = mb.mb_presence in
  let mb_expr = of_module_expr mb.mb_expr in
  let mb_attributes = of_attributes mb.mb_attributes in
  let mb_loc = mb.mb_loc in
  { mb_id; mb_name; mb_presence; mb_expr; mb_attributes; mb_loc }

and of_value_binding : OCaml.value_binding -> value_binding = fun vb ->
  let vb_pat = of_pattern vb.vb_pat in
  let vb_expr = of_expression vb.vb_expr in
  let vb_attributes = of_attributes vb.vb_attributes in
  let vb_loc = vb.vb_loc in
  { vb_pat; vb_expr; vb_attributes; vb_loc }

and of_module_coercion : OCaml.module_coercion -> module_coercion = function
  | Tcoerce_none -> Tcoerce_none
  | Tcoerce_structure (pos_coercions, id_pos_list) ->
      let pos_coercions =
        let convert (i, mod_coer) =
          let mod_coer' = of_module_coercion mod_coer in
          (i, mod_coer')
        in
        List.map convert pos_coercions
      in
      let id_pos_list =
        let convert (id, i, mod_coer) =
          let mod_coer' = of_module_coercion mod_coer in
          (id, i, mod_coer')
        in
        List.map convert id_pos_list
      in
      Tcoerce_structure { pos_coercions; id_pos_list }
  | Tcoerce_functor (arg_coercion, res_coercion) ->
      let arg_coercion = of_module_coercion arg_coercion in
      let res_coercion = of_module_coercion res_coercion in
      Tcoerce_functor { arg_coercion; res_coercion }
  | Tcoerce_primitive coercion ->
      let coercion = of_primitive_coercion coercion in
      Tcoerce_primitive { coercion }
  | Tcoerce_alias (env, path, coercion) ->
      let coercion = of_module_coercion coercion in
      Tcoerce_alias { env; path; coercion }

and of_module_type : OCaml.module_type -> module_type = fun mty ->
    let mty_desc = of_module_type_desc mty.mty_desc in
    let mty_type = mty.mty_type  in
    let mty_env = mty.mty_env  in
    let mty_loc = mty.mty_loc in
    let mty_attributes = of_attributes mty.mty_attributes in
    { mty_desc; mty_type; mty_env; mty_loc; mty_attributes }

and of_module_type_desc : OCaml.module_type_desc -> module_type_desc = function
  | Tmty_ident (path, longid) -> Tmty_ident { path; longid }
  | Tmty_signature sign ->
      let sign = of_signature sign in
      Tmty_signature { sign }
  | Tmty_functor (param, res_type) ->
      let param = of_functor_parameter param in
      let res_type = of_module_type res_type in
      Tmty_functor { param; res_type }
  | Tmty_with (mod_type, constraints) ->
      let mod_type = of_module_type mod_type in
      let constraints =
        let convert (path, longid, wc) =
          let wc' = of_with_constraint wc in
          (path, longid, wc')
        in
        List.map convert constraints
      in
      Tmty_with { mod_type; constraints }
  | Tmty_typeof mod_expr ->
      let mod_expr = of_module_expr mod_expr in
      Tmty_typeof { mod_expr }
  | Tmty_alias (path, longid) -> Tmty_alias { path; longid }

and of_primitive_coercion : OCaml.primitive_coercion -> primitive_coercion =
  fun pc ->
  let pc_desc = pc.pc_desc in
  let pc_type = pc.pc_type in
  let pc_env = pc.pc_env in
  let pc_loc = pc.pc_loc in
  { pc_desc; pc_type; pc_env; pc_loc }

and of_signature : OCaml.signature -> signature = fun sign ->
  let sig_items = List.map of_signature_item sign.sig_items in
  let sig_type = sign.sig_type in
  let sig_final_env = sign.sig_final_env in
  { sig_items; sig_type; sig_final_env }

and of_signature_item : OCaml.signature_item -> signature_item = fun sign ->
  let sig_desc = of_signature_item_desc sign.sig_desc in
  let sig_env = sign.sig_env in
  let sig_loc = sign.sig_loc in
  { sig_desc; sig_env; sig_loc }

and of_signature_item_desc : OCaml.signature_item_desc -> signature_item_desc =
  function
  | Tsig_value val_desc ->
      let val_desc = of_value_description val_desc in
      Tsig_value { val_desc }
  | Tsig_type (rec_, type_decls) ->
      let type_decls = List.map of_type_declaration type_decls in
      Tsig_type { rec_; type_decls }
  | Tsig_typesubst type_decls ->
      let type_decls = List.map of_type_declaration type_decls in
      Tsig_typesubst { type_decls }
  | Tsig_typext type_ext ->
      let type_ext = of_type_extension type_ext in
      Tsig_typext { type_ext }
  | Tsig_exception type_exc ->
      let type_exc = of_type_exception type_exc in
      Tsig_exception { type_exc }
  | Tsig_module mod_decl ->
      let mod_decl = of_module_declaration mod_decl in
      Tsig_module { mod_decl }
  | Tsig_modsubst mod_subst ->
      let mod_subst = of_module_substitution mod_subst in
      Tsig_modsubst { mod_subst }
  | Tsig_recmodule mod_delcs ->
      let mod_delcs = List.map of_module_declaration mod_delcs in
      Tsig_recmodule { mod_delcs }
  | Tsig_modtype modtype_decl ->
      let modtype_decl = of_module_type_declaration modtype_decl in
      Tsig_modtype { modtype_decl }
  | Tsig_modtypesubst modtype_decl ->
      let modtype_decl = of_module_type_declaration modtype_decl in
      Tsig_modtypesubst { modtype_decl }
  | Tsig_open open_desc ->
      let open_desc = of_open_description open_desc in
      Tsig_open { open_desc }
  | Tsig_include incl_desc ->
      let incl_desc = of_include_description incl_desc in
      Tsig_include { incl_desc }
  | Tsig_class class_descs ->
      let class_descs = List.map of_class_description class_descs in
      Tsig_class { class_descs }
  | Tsig_class_type classtype_decls ->
      let classtype_decls =
        List.map of_class_type_declaration classtype_decls
      in
      Tsig_class_type { classtype_decls }
  | Tsig_attribute attribute ->
      let attribute = of_attribute attribute in
     Tsig_attribute { attribute }

and of_module_declaration : OCaml.module_declaration -> module_declaration =
  fun md ->
  let md_id = md.md_id in
  let md_name = md.md_name in
  let md_presence = md.md_presence in
  let md_type = of_module_type md.md_type in
  let md_attributes = of_attributes md.md_attributes in
  let md_loc = md.md_loc in
  { md_id; md_name; md_presence; md_type; md_attributes; md_loc }

and of_module_substitution : OCaml.module_substitution -> module_substitution =
  fun ms ->
  let ms_id = ms.ms_id in
  let ms_name = ms.ms_name in
  let ms_manifest = ms.ms_manifest in
  let ms_txt = ms.ms_txt in
  let ms_attributes = of_attributes ms.ms_attributes in
  let ms_loc = ms.ms_loc in
  { ms_id; ms_name; ms_manifest; ms_txt; ms_attributes; ms_loc }

and of_module_type_declaration:
  OCaml.module_type_declaration -> module_type_declaration =
  fun mtd ->
  let mtd_id = mtd.mtd_id in
  let mtd_name = mtd.mtd_name in
  let mtd_type = Option.map of_module_type mtd.mtd_type in
  let mtd_attributes = of_attributes mtd.mtd_attributes in
  let mtd_loc = mtd.mtd_loc in
  { mtd_id; mtd_name; mtd_type; mtd_attributes; mtd_loc }

and of_open_infos:
  type a b . of_open_expr:(a -> b) -> a OCaml.open_infos -> b open_infos =
  fun ~of_open_expr oi ->
  let open_expr = of_open_expr oi.open_expr in
  let open_bound_items = oi.open_bound_items in
  let open_override = oi.open_override in
  let open_env = oi.open_env in
  let open_loc = oi.open_loc in
  let open_attributes = of_attributes oi.open_attributes in
  { open_expr; open_bound_items; open_override; open_env; open_loc; open_attributes }

and of_open_description : OCaml.open_description -> open_description =
  fun open_desc ->
  of_open_infos ~of_open_expr:Fun.id open_desc

and of_open_declaration : OCaml.open_declaration -> open_declaration =
  fun open_decl ->
  of_open_infos ~of_open_expr:of_module_expr open_decl

and of_include_infos:
  type a b . of_incl_mod:(a -> b) -> a OCaml.include_infos -> b include_infos =
  fun ~of_incl_mod ii ->
  let incl_mod = of_incl_mod ii.incl_mod in
  let incl_type = ii.incl_type in
  let incl_loc = ii.incl_loc in
  let incl_attributes = of_attributes ii.incl_attributes in
  { incl_mod; incl_type; incl_loc; incl_attributes }

and of_include_description : OCaml.include_description -> include_description =
  fun incl_desc ->
  of_include_infos ~of_incl_mod:of_module_type incl_desc

and of_include_declaration : OCaml.include_declaration -> include_declaration =
  fun incl_decl ->
  of_include_infos ~of_incl_mod:of_module_expr incl_decl

and of_with_constraint : OCaml.with_constraint -> with_constraint = function
  | Twith_type type_decl ->
      let type_decl = of_type_declaration type_decl in
      Twith_type { type_decl }
  | Twith_module (path, longid) -> Twith_module { path; longid }
  | Twith_modtype mod_type ->
      let mod_type = of_module_type mod_type in
      Twith_modtype { mod_type }
  | Twith_typesubst type_decl ->
      let type_decl = of_type_declaration type_decl in
      Twith_typesubst { type_decl }
  | Twith_modsubst (path, longid) -> Twith_modsubst { path; longid }
  | Twith_modtypesubst mod_type ->
      let mod_type = of_module_type mod_type in
      Twith_modtypesubst { mod_type }

and of_core_type : OCaml.core_type -> core_type = fun ct ->
    let ctyp_desc = of_core_type_desc ct.ctyp_desc in
    let ctyp_type = ct.ctyp_type in
    let ctyp_env = ct.ctyp_env in
    let ctyp_loc = ct.ctyp_loc in
    let ctyp_attributes = of_attributes ct.ctyp_attributes in
    { ctyp_desc; ctyp_type; ctyp_env; ctyp_loc; ctyp_attributes }

and of_core_type_desc : OCaml.core_type_desc -> core_type_desc = function
  | Ttyp_any -> Ttyp_any
  | Ttyp_var name -> Ttyp_var { name }
  | Ttyp_arrow (arg_label, arg_type, res_type) ->
      let arg_type = of_core_type arg_type in
      let res_type = of_core_type res_type in
      Ttyp_arrow { arg_label; arg_type; res_type }
  | Ttyp_tuple fields ->
      let fields = List.map of_core_type fields in
      Ttyp_tuple { fields }
  | Ttyp_constr (path, longid, params) ->
      let params = List.map of_core_type params in
      Ttyp_constr { path; longid; params }
  | Ttyp_object (fields, closed) ->
      let fields = List.map of_object_field fields in
      Ttyp_object { fields; closed }
  | Ttyp_class (path, longid, params) ->
      let params = List.map of_core_type params in
      Ttyp_class { path; longid; params }
  | Ttyp_alias (type_, name) ->
      let type_ = of_core_type type_ in
      Ttyp_alias { type_; name }
  | Ttyp_variant (rows, closed, labels) ->
      let rows = List.map of_row_field rows in
      Ttyp_variant { rows; closed; labels }
  | Ttyp_poly (params, type_) ->
      let type_ = of_core_type type_ in
      Ttyp_poly { params; type_ }
  | Ttyp_package (pack_type) ->
      let pack_type = of_package_type pack_type in
      Ttyp_package { pack_type }

and of_package_type : OCaml.package_type -> package_type = fun pt ->
  let pack_path = pt.pack_path in
  let pack_fields =
    let convert (longid, t) =
      let t' = of_core_type t in
      (longid, t')
    in
    List.map convert pt.pack_fields
  in
  let pack_type = pt.pack_type in
  let pack_txt = pt.pack_txt in
  { pack_path; pack_fields; pack_type; pack_txt }

and of_row_field : OCaml.row_field -> row_field = fun rf ->
  let rf_desc = of_row_field_desc rf.rf_desc in
  let rf_loc = rf.rf_loc in
  let rf_attributes = of_attributes rf.rf_attributes in
  { rf_desc; rf_loc; rf_attributes }

and of_row_field_desc : OCaml.row_field_desc -> row_field_desc = function
  | Ttag (name, empty, conj) ->
      let conj = List.map of_core_type conj in
      Ttag { name; empty; conj }
  | Tinherit type_ ->
      let type_ = of_core_type type_ in
      Tinherit { type_ }

and of_object_field : OCaml.object_field -> object_field = fun obf ->
  let of_desc = of_object_field_desc obf.of_desc in
  let of_loc = obf.of_loc in
  let of_attributes = of_attributes obf.of_attributes in
  { of_desc; of_loc; of_attributes }

and of_object_field_desc : OCaml.object_field_desc -> object_field_desc =
  function
  | OTtag (name, type_) ->
      let type_ = of_core_type type_ in
      OTtag { name; type_ }
  | OTinherit type_ ->
      let type_ = of_core_type type_ in
      OTinherit { type_ }

and of_value_description : OCaml.value_description -> value_description =
  fun vd ->
  let val_id = vd.val_id in
  let val_name = vd.val_name in
  let val_desc = of_core_type vd.val_desc in
  let val_val = vd.val_val in
  let val_prim = vd.val_prim in
  let val_loc = vd.val_loc in
  let val_attributes = of_attributes vd.val_attributes in
  { val_id; val_name; val_desc; val_val; val_prim; val_loc; val_attributes }

and of_type_declaration : OCaml.type_declaration -> type_declaration = fun td ->
  let typ_id = td.typ_id in
  let typ_name = td.typ_name in
  let typ_params =
    let convert (t, varinj) =
      let t' = of_core_type t in
      (t', varinj)
    in
    List.map convert td.typ_params
  in
  let typ_type = td.typ_type in
  let typ_cstrs =
    let convert (t1, t2, loc) =
      let t1' = of_core_type t1 in
      let t2' = of_core_type t2 in
      (t1', t2', loc)
    in
    List.map convert td.typ_cstrs
  in
  let typ_kind = of_type_kind td.typ_kind in
  let typ_private = td.typ_private in
  let typ_manifest = Option.map of_core_type td.typ_manifest in
  let typ_loc = td.typ_loc in
  let typ_attributes = of_attributes td.typ_attributes in
  { typ_id;
    typ_name;
    typ_params;
    typ_type;
    typ_cstrs;
    typ_kind;
    typ_private;
    typ_manifest;
    typ_loc;
    typ_attributes;
  }

and of_type_kind : OCaml.type_kind -> type_kind = function
  | Ttype_abstract ->Ttype_abstract
  | Ttype_variant ctor_decls ->
      let ctor_decls = List.map of_constructor_declaration ctor_decls in
      Ttype_variant { ctor_decls }
  | Ttype_record label_decls ->
      let label_decls = List.map of_label_declaration label_decls in
      Ttype_record { label_decls }
  | Ttype_open -> Ttype_open

and of_label_declaration : OCaml.label_declaration -> label_declaration =
  fun ld ->
  let ld_id = ld.ld_id in
  let ld_name = ld.ld_name in
  let ld_mutable = ld.ld_mutable in
  let ld_type = of_core_type ld.ld_type in
  let ld_loc = ld.ld_loc in
  let ld_attributes = of_attributes ld.ld_attributes in
  { ld_id; ld_name; ld_mutable; ld_type; ld_loc; ld_attributes }

and of_constructor_declaration :
  OCaml.constructor_declaration -> constructor_declaration =
  fun cd ->
  let cd_id = cd.cd_id in
  let cd_name = cd.cd_name in
  let cd_vars = cd.cd_vars in
  let cd_args = of_constructor_arguments cd.cd_args in
  let cd_res = Option.map of_core_type cd.cd_res in
  let cd_loc = cd.cd_loc in
  let cd_attributes = of_attributes cd.cd_attributes in
  { cd_id; cd_name; cd_vars; cd_args; cd_res; cd_loc; cd_attributes }

and of_constructor_arguments :
  OCaml.constructor_arguments -> constructor_arguments =
  function
  | Cstr_tuple fields ->
      let fields = List.map of_core_type fields in
      Cstr_tuple { fields }
  | Cstr_record label_decls ->
      let label_decls = List.map of_label_declaration label_decls in
      Cstr_record { label_decls }

and of_type_extension : OCaml.type_extension -> type_extension = fun te ->
  let tyext_path = te.tyext_path in
  let tyext_txt = te.tyext_txt in
  let tyext_params =
    let convert (t, varinj) =
      let t' = of_core_type t in
      (t', varinj)
    in
    List.map convert te.tyext_params
  in
  let tyext_constructors =
    List.map of_extension_constructor te.tyext_constructors
  in
  let tyext_private = te.tyext_private in
  let tyext_loc = te.tyext_loc in
  let tyext_attributes = of_attributes te.tyext_attributes in
  { tyext_path;
    tyext_txt;
    tyext_params;
    tyext_constructors;
    tyext_private;
    tyext_loc;
    tyext_attributes;
  }

and of_type_exception : OCaml.type_exception -> type_exception = fun te ->
  let tyexn_constructor = of_extension_constructor te.tyexn_constructor in
  let tyexn_loc = te.tyexn_loc in
  let tyexn_attributes = of_attributes te.tyexn_attributes in
  { tyexn_constructor; tyexn_loc; tyexn_attributes }

and of_extension_constructor :
  OCaml.extension_constructor -> extension_constructor =
  fun ec ->
  let ext_id = ec.ext_id in
  let ext_name = ec.ext_name in
  let ext_type = ec.ext_type in
  let ext_kind = of_extension_constructor_kind ec.ext_kind in
  let ext_loc = ec.ext_loc in
  let ext_attributes = of_attributes ec.ext_attributes in
  { ext_id; ext_name; ext_type; ext_kind; ext_loc; ext_attributes }

and of_extension_constructor_kind :
  OCaml.extension_constructor_kind -> extension_constructor_kind =
  function
  | Text_decl (existentials, arg, res_type) ->
      let arg = of_constructor_arguments arg in
      let res_type = Option.map of_core_type res_type in
      Text_decl { existentials; arg; res_type }
  | Text_rebind (path, longid) -> Text_rebind { path; longid }

and of_class_type : OCaml.class_type -> class_type = fun ct ->
  let cltyp_desc = of_class_type_desc ct.cltyp_desc in
  let cltyp_type = ct.cltyp_type in
  let cltyp_env = ct.cltyp_env in
  let cltyp_loc = ct.cltyp_loc in
  let cltyp_attributes = of_attributes ct.cltyp_attributes in
  { cltyp_desc; cltyp_type; cltyp_env; cltyp_loc; cltyp_attributes }

and of_class_type_desc : OCaml.class_type_desc -> class_type_desc = function
  | Tcty_constr (path, longid, params) ->
      let params = List.map of_core_type params in
      Tcty_constr { path; longid; params }
  | Tcty_signature class_sign ->
      let class_sign = of_class_signature class_sign in
      Tcty_signature { class_sign }
  | Tcty_arrow (arg_label, arg_type, class_type) ->
      let arg_type = of_core_type arg_type in
      let class_type = of_class_type class_type in
      Tcty_arrow { arg_label; arg_type; class_type }
  | Tcty_open (open_desc, class_type) ->
      let open_desc = of_open_description open_desc in
      let class_type = of_class_type class_type in
      Tcty_open { open_desc; class_type }

and of_class_signature : OCaml.class_signature -> class_signature = fun cs ->
  let csig_self = of_core_type cs.csig_self in
  let csig_fields = List.map of_class_type_field cs.csig_fields in
  let csig_type = cs.csig_type in
  { csig_self; csig_fields; csig_type }

and of_class_type_field : OCaml.class_type_field -> class_type_field = fun cf ->
  let ctf_desc = of_class_type_field_desc cf.ctf_desc in
  let ctf_loc = cf.ctf_loc in
  let ctf_attributes = of_attributes cf.ctf_attributes in
  { ctf_desc; ctf_loc; ctf_attributes }

and of_class_type_field_desc :
  OCaml.class_type_field_desc -> class_type_field_desc =
  function
  | Tctf_inherit parent_type ->
      let parent_type = of_class_type parent_type in
      Tctf_inherit { parent_type }
  | Tctf_val (name, mut, virt, type_) ->
      let type_ = of_core_type type_ in
      Tctf_val { name; mut; virt; type_ }
  | Tctf_method (name, priv, virt, type_) ->
      let type_ = of_core_type type_ in
      Tctf_method { name; priv; virt; type_ }
  | Tctf_constraint (type1, type2) ->
      let type1 = of_core_type type1 in
      let type2 = of_core_type type2 in
      Tctf_constraint { type1; type2 }
  | Tctf_attribute attribute ->
      let attribute = of_attribute attribute in
      Tctf_attribute { attribute }

and of_class_declaration : OCaml.class_declaration -> class_declaration =
  fun class_decl ->
  of_class_infos ~of_ci_expr:of_class_expr class_decl

and of_class_description : OCaml.class_description -> class_description =
  fun class_desc ->
  of_class_infos ~of_ci_expr:of_class_type class_desc

and of_class_type_declaration : OCaml.class_type_declaration -> class_type_declaration =
  fun clty_decl ->
  of_class_infos ~of_ci_expr:of_class_type clty_decl

and of_class_infos :
  type a b . of_ci_expr:(a -> b) -> a OCaml.class_infos -> b class_infos =
  fun ~of_ci_expr ci ->
  let ci_virt = ci.ci_virt in
  let ci_params =
    let convert (t, varinj) =
      let t' = of_core_type t in
      (t', varinj)
    in
   List.map convert ci.ci_params
  in
  let ci_id_name = ci.ci_id_name in
  let ci_id_class = ci.ci_id_class in
  let ci_id_class_type = ci.ci_id_class_type in
  let ci_id_object = ci.ci_id_object in
  let ci_id_typehash = ci.ci_id_typehash in
  let ci_expr = of_ci_expr ci.ci_expr in
  let ci_decl = ci.ci_decl in
  let ci_type_decl = ci.ci_type_decl in
  let ci_loc = ci.ci_loc in
  let ci_attributes = of_attributes ci.ci_attributes in
  { ci_virt;
    ci_params;
    ci_id_name;
    ci_id_class;
    ci_id_class_type;
    ci_id_object;
    ci_id_typehash;
    ci_expr;
    ci_decl;
    ci_type_decl;
    ci_loc;
    ci_attributes;
  }


(* [to_*] conversion functions *)

let to_partial : partial -> OCaml.partial = function
  | Partial -> Partial
  | Total -> Total

let to_attribute : attribute -> OCaml.attribute = Fun.id

let to_attributes : attributes -> OCaml.attributes = Fun.id

let to_value : value -> OCaml.value = Fun.id

let to_computation : computation -> OCaml.computation = Fun.id

let rec to_pattern : pattern -> OCaml.pattern = fun pat ->
  to_general_pattern pat

and to_general_pattern : type k . k general_pattern -> k OCaml.general_pattern = fun pat ->
  to_pattern_data ~to_pat_desc:to_pattern_desc pat

and to_pattern_data : type a b . to_pat_desc:(a -> b) -> a pattern_data -> b OCaml.pattern_data =
  fun ~to_pat_desc pat_data ->
  let pat_desc = to_pat_desc pat_data.pat_desc in
  let pat_loc = pat_data.pat_loc in
  let pat_extra =
    let convert_of (px, loc, attrs) =
      let px' = to_pat_extra px in
      let attrs' = to_attributes attrs in
      (px', loc, attrs')
    in
    List.map convert_of pat_data.pat_extra
  in
  let pat_type = pat_data.pat_type in
  let pat_env = pat_data.pat_env in
  let pat_attributes = to_attributes pat_data.pat_attributes in
  { pat_desc; pat_loc; pat_extra; pat_type; pat_env; pat_attributes }

and to_pat_extra : pat_extra -> OCaml.pat_extra = function
  | Tpat_constraint { type_ } ->
      let type_ = to_core_type type_ in
      Tpat_constraint type_
  | Tpat_type { path; longid } -> Tpat_type (path, longid)
  | Tpat_open { path; longid; env } -> Tpat_open (path, longid, env)
  | Tpat_unpack -> Tpat_unpack

and to_pattern_desc : type k . k pattern_desc -> k OCaml.pattern_desc = function
  (* value patterns *)
  | Tpat_any -> Tpat_any
  | Tpat_var { id; name } -> Tpat_var (id, name)
  | Tpat_alias { pat; id; name } ->
      let pat = to_general_pattern pat in
      Tpat_alias (pat, id, name)
  | Tpat_constant { const } -> Tpat_constant const
  | Tpat_tuple { fields } ->
      let fields = List.map to_general_pattern fields in
      Tpat_tuple fields
  | Tpat_construct { longid; ctor_desc; fields; typing } ->
      let fields = List.map to_general_pattern fields in
      let typing =
        let convert (ids, t) =
          let t' = to_core_type t in
          (ids, t')
        in
        Option.map convert typing
      in
      Tpat_construct (longid, ctor_desc, fields, typing)
  | Tpat_variant { label; pat; row_desc } ->
      let pat = Option.map to_general_pattern pat in
      (* XXX: reusing the same row_desc, which is a ref *)
      Tpat_variant (label, pat, row_desc)
  | Tpat_record { fields; closed } ->
      let fields =
        let convert (id, lab, pat) =
          let pat' = to_general_pattern pat in
          (id, lab, pat')
        in
        List.map convert fields
      in
      Tpat_record (fields, closed)
  | Tpat_array { cells } ->
      let cells = List.map to_general_pattern cells in
      Tpat_array cells
  | Tpat_lazy { pat } ->
      let pat = to_general_pattern pat in
      Tpat_lazy pat
  (* computation patterns *)
  | Tpat_value { pat } ->
      let pat = to_tpat_value_argument pat in
      Tpat_value pat
  | Tpat_exception { pat } ->
      let pat = to_general_pattern pat in
      Tpat_exception pat
  (* generic constructions *)
  | Tpat_or { left_pat; right_pat; row_desc } ->
      let left_pat = to_general_pattern left_pat in
      let right_pat = to_general_pattern right_pat in
      Tpat_or (left_pat, right_pat, row_desc)

and to_tpat_value_argument : tpat_value_argument -> OCaml.tpat_value_argument =
  fun pat ->
    let value_pat = to_pattern pat in
    let computation_pat : OCaml.computation OCaml.general_pattern =
      OCaml.as_computation_pattern value_pat
    in
    match computation_pat.pat_desc with
    | Tpat_value pat -> pat
    | Tpat_exception _ | Tpat_or _ -> assert false
    | _ -> .

and to_expression : expression -> OCaml.expression = fun expr ->
  let exp_desc = to_expression_desc expr.exp_desc in
  let exp_loc = expr.exp_loc in
  let exp_extra =
    let convert_of (ex, loc, attrs) =
      let ex' = to_exp_extra ex in
      let attrs' = to_attributes attrs in
      (ex', loc, attrs')
    in
    List.map convert_of expr.exp_extra
  in
  let exp_type = expr.exp_type in
  let exp_env = expr.exp_env in
  let exp_attributes = to_attributes expr.exp_attributes in
  { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes }

and to_exp_extra : exp_extra -> OCaml.exp_extra = function
  | Texp_constraint { type_ } ->
      let type_ = to_core_type type_ in
      Texp_constraint (type_)
  | Texp_coerce { from_type; to_type } ->
      let from_type = Option.map to_core_type from_type in
      let to_type = to_core_type to_type in
      Texp_coerce (from_type, to_type)
  | Texp_poly { type_ } ->
      let type_ = Option.map to_core_type type_ in
      Texp_poly (type_)
  | Texp_newtype { name } -> Texp_newtype (name)

and to_expression_desc : expression_desc -> OCaml.expression_desc = function
  | Texp_ident { path; longid; value_desc } ->
      Texp_ident (path, longid, value_desc)
  | Texp_constant { const } -> Texp_constant const
  | Texp_let { rec_; bindings; in_ } ->
      let bindings = List.map to_value_binding bindings in
      let in_ = to_expression in_ in
      Texp_let (rec_, bindings, in_)
  | Texp_function { arg_label; param; cases; partial } ->
      let cases = List.map to_case cases in
      let partial = to_partial partial in
      Texp_function { arg_label; param; cases; partial }
  | Texp_apply { f; args } ->
      let f = to_expression f in
      let args =
        let convert (lab, expr) =
          let expr = Option.map to_expression expr in
          (lab, expr)
        in
        List.map convert args
      in
      Texp_apply (f, args)
  | Texp_match { expr; cases; partial } ->
      let expr = to_expression expr in
      let cases = List.map to_case cases in
      let partial = to_partial partial in
      Texp_match (expr, cases, partial)
  | Texp_try { expr; cases } ->
      let expr = to_expression expr in
      let cases = List.map to_case cases in
      Texp_try (expr, cases)
  | Texp_tuple { fields } ->
      let fields = List.map to_expression fields in
      Texp_tuple fields
  | Texp_construct { longid; ctor_desc; fields } ->
      let fields = List.map to_expression fields in
      Texp_construct (longid, ctor_desc, fields)
  | Texp_variant { label; expr } ->
      let expr = Option.map to_expression expr in
      Texp_variant (label, expr)
  | Texp_record { fields; representation; extended_expression } ->
      let fields =
        let convert (lab, rld) =
          let rld' = to_record_label_definition rld in
          (lab, rld')
        in
        Array.map convert fields
      in
      let extended_expression = Option.map to_expression extended_expression in
      Texp_record { fields; representation; extended_expression }
  | Texp_field { record; longid; desc } ->
      let record = to_expression record in
      Texp_field (record, longid, desc)
  | Texp_setfield { record; longid; desc; expr } ->
      let record = to_expression record in
      let expr = to_expression expr in
      Texp_setfield (record, longid, desc, expr)
  | Texp_array { cells } ->
      let cells = List.map to_expression cells in
      Texp_array cells
  | Texp_ifthenelse { cond; then_; else_} ->
      let cond = to_expression cond in
      let then_ = to_expression then_ in
      let else_ = Option.map to_expression else_ in
      Texp_ifthenelse (cond, then_, else_)
  | Texp_sequence { expr1; expr2 } ->
      let expr1 = to_expression expr1 in
      let expr2 = to_expression expr2 in
      Texp_sequence (expr1, expr2)
  | Texp_while { cond; body } ->
      let cond = to_expression cond in
      let body = to_expression body in
      Texp_while (cond, body)
  | Texp_for { counter_id; counter_pat; start; finish; direction; body } ->
      let start = to_expression start in
      let finish = to_expression finish in
      let body = to_expression body in
      Texp_for (counter_id, counter_pat, start, finish, direction, body)
  | Texp_send { obj; meth } ->
      let obj = to_expression obj in
      let meth = to_meth meth in
      Texp_send (obj, meth)
  | Texp_new { path; longid; class_decl } -> Texp_new (path, longid, class_decl)
  | Texp_instvar { class_path; var_path; name } ->
      Texp_instvar (class_path, var_path, name)
  | Texp_setinstvar { class_path; var_path; name; expr } ->
      let expr = to_expression expr in
      Texp_setinstvar (class_path, var_path, name, expr)
  | Texp_override { class_path; instvar_changes } ->
      let instvar_changes =
        let convert (id, name, expr) =
          let expr' = to_expression expr in
          (id, name, expr')
        in
        List.map convert instvar_changes
      in
      Texp_override (class_path, instvar_changes)
  | Texp_letmodule { id; name; presence; mod_expr; in_ } ->
      let mod_expr = to_module_expr mod_expr in
      let in_ = to_expression in_ in
      Texp_letmodule (id, name, presence, mod_expr, in_)
  | Texp_letexception { extension_ctor; in_ } ->
      let extension_ctor = to_extension_constructor extension_ctor in
      let in_ = to_expression in_ in
      Texp_letexception (extension_ctor, in_)
  | Texp_assert { expr } ->
      let expr = to_expression expr in
      Texp_assert expr
  | Texp_lazy { expr } ->
      let expr = to_expression expr in
      Texp_lazy expr
  | Texp_object { class_strc; meths } ->
      let class_strc = to_class_structure class_strc in
      Texp_object (class_strc, meths)
  | Texp_pack { mod_expr } ->
      let mod_expr = to_module_expr mod_expr in
      Texp_pack mod_expr
  | Texp_letop { let_; ands; param; body; partial } ->
      let let_ = to_binding_op let_ in
      let ands = List.map to_binding_op ands in
      let body = to_case body in
      let partial = to_partial partial in
      Texp_letop { let_; ands; param; body; partial }
  | Texp_unreachable -> Texp_unreachable
  | Texp_extension_constructor { longid; path } ->
      Texp_extension_constructor (longid, path)
  | Texp_open { open_decl; in_ } ->
      let open_decl = to_open_declaration open_decl in
      let in_ = to_expression in_ in
      Texp_open (open_decl, in_)

and to_meth : meth -> OCaml.meth = function
  | Tmeth_name { name } -> Tmeth_name name
  | Tmeth_val { id } -> Tmeth_val id
  | Tmeth_ancestor { id; path } -> Tmeth_ancestor (id, path)

and to_case : 'k . 'k case -> 'k OCaml.case = fun case ->
  let c_lhs = to_general_pattern case.c_lhs in
  let c_guard = Option.map to_expression case.c_guard in
  let c_rhs = to_expression case.c_rhs in
  { c_lhs; c_guard; c_rhs }

and to_record_label_definition :
  record_label_definition -> OCaml.record_label_definition =
  function
  | Kept { type_expr } -> Kept type_expr
  | Overridden { longid; expr } ->
      let expr = to_expression expr in
      Overridden (longid, expr)

and to_binding_op : binding_op -> OCaml.binding_op = fun bop ->
  let bop_op_path = bop.bop_op_path in
  let bop_op_name = bop.bop_op_name in
  let bop_op_val = bop.bop_op_val in
  let bop_op_type = bop.bop_op_type in
  let bop_exp = to_expression bop.bop_exp in
  let bop_loc = bop.bop_loc in
  { bop_op_path; bop_op_name; bop_op_val; bop_op_type; bop_exp; bop_loc }

and to_class_expr : class_expr -> OCaml.class_expr = fun cl ->
  let cl_desc = to_class_expr_desc cl.cl_desc in
  let cl_loc = cl.cl_loc in
  let cl_type = cl.cl_type in
  let cl_env = cl.cl_env in
  let cl_attributes = to_attributes cl.cl_attributes in
  { cl_desc; cl_loc; cl_type; cl_env; cl_attributes }

and to_class_expr_desc : class_expr_desc -> OCaml.class_expr_desc = function
  | Tcl_ident { path; longid; params } ->
        let params = List.map to_core_type params in
        Tcl_ident (path, longid, params)
  | Tcl_structure { strc } ->
      let strc = to_class_structure strc in
      Tcl_structure strc
  | Tcl_fun { arg_label; arg_pattern; arg_pattern_vars; body; partial } ->
      let arg_pattern = to_pattern arg_pattern in
      let arg_pattern_vars =
        let convert (id, expr) =
          let expr' = to_expression expr in
          (id, expr')
        in
        List.map convert arg_pattern_vars
      in
      let body = to_class_expr body in
      let partial = to_partial partial in
      Tcl_fun (arg_label, arg_pattern, arg_pattern_vars, body, partial)
  | Tcl_apply { c; args } ->
      let c = to_class_expr c in
      let args =
        let convert (lab, expr) =
          let expr' = Option.map to_expression expr in
          (lab, expr')
        in
        List.map convert args
      in
      Tcl_apply (c, args)
  | Tcl_let { rec_; bindings; vars; class_expr } ->
      let bindings = List.map to_value_binding bindings in
      let vars =
        let convert (id, expr) =
          let expr' = to_expression expr in
          (id, expr')
        in
        List.map convert vars
      in
      let class_expr = to_class_expr class_expr in
      Tcl_let (rec_, bindings, vars, class_expr)
  | Tcl_constraint { class_expr; class_type; instvars; meths; concrete_meths } ->
      let class_expr = to_class_expr class_expr in
      let class_type = Option.map to_class_type class_type in
      Tcl_constraint (class_expr, class_type, instvars, meths, concrete_meths)
  | Tcl_open { open_desc; in_ } ->
      let open_desc = to_open_description open_desc in
      let in_ = to_class_expr in_ in
      Tcl_open (open_desc, in_)

and to_class_structure : class_structure -> OCaml.class_structure = fun cstr ->
  let cstr_self = to_pattern cstr.cstr_self in
  let cstr_fields = List.map to_class_field cstr.cstr_fields in
  let cstr_type = cstr.cstr_type in
  let cstr_meths = cstr.cstr_meths in
  { cstr_self; cstr_fields; cstr_type; cstr_meths }

and to_class_field : class_field -> OCaml.class_field = fun cf ->
  let cf_desc = to_class_field_desc cf.cf_desc in
  let cf_loc = cf.cf_loc in
  let cf_attributes = to_attributes cf.cf_attributes in
  { cf_desc; cf_loc; cf_attributes }

and to_class_field_kind : class_field_kind -> OCaml.class_field_kind = function
  | Tcfk_virtual { type_ } ->
      let type_ = to_core_type type_ in
      Tcfk_virtual type_
  | Tcfk_concrete{ over; expr } ->
      let expr = to_expression expr in
      Tcfk_concrete (over, expr)

and to_class_field_desc : class_field_desc -> OCaml.class_field_desc = function
  | Tcf_inherit { over; parent_class; parent_alias; instvars; meths } ->
      let parent_class = to_class_expr parent_class in
      Tcf_inherit (over, parent_class, parent_alias, instvars, meths)
  | Tcf_val { name; mut; id; virt; already_declared } ->
      let virt = to_class_field_kind virt in
      Tcf_val (name, mut, id, virt, already_declared)
  | Tcf_method { name; priv; virt } ->
      let virt = to_class_field_kind virt in
      Tcf_method (name, priv, virt)
  | Tcf_constraint { type1; type2 } ->
      let type1 = to_core_type type1 in
      let type2 = to_core_type type2 in
      Tcf_constraint (type1, type2)
  | Tcf_initializer { expr } ->
      let expr = to_expression expr in
      Tcf_initializer expr
  | Tcf_attribute { attribute } ->
      let attribute = to_attribute attribute in
      Tcf_attribute attribute

and to_module_expr : module_expr -> OCaml.module_expr = fun me ->
    let mod_desc = to_module_expr_desc me.mod_desc in
    let mod_loc = me.mod_loc in
    let mod_type = me.mod_type in
    let mod_env = me.mod_env in
    let mod_attributes = to_attributes me.mod_attributes in
    { mod_desc; mod_loc; mod_type; mod_env; mod_attributes }

and to_module_type_constraint:
  module_type_constraint -> OCaml.module_type_constraint =
  function
  | Tmodtype_implicit -> Tmodtype_implicit
  | Tmodtype_explicit { mod_type } ->
      let mod_type = to_module_type mod_type in
      Tmodtype_explicit mod_type

and to_functor_parameter : functor_parameter -> OCaml.functor_parameter = function
  | Unit -> Unit
  | Named { id; name; mod_type } ->
      let mod_type = to_module_type mod_type in
      Named (id, name, mod_type)

and to_module_expr_desc : module_expr_desc -> OCaml.module_expr_desc = function
  | Tmod_ident { path; longid } -> Tmod_ident (path, longid)
  | Tmod_structure { strc } ->
      let strc = to_structure strc in
      Tmod_structure strc
  | Tmod_functor { param; body } ->
      let param = to_functor_parameter param in
      let body = to_module_expr body in
      Tmod_functor (param, body)
  | Tmod_apply { ftor; arg; res_coercion } ->
      let ftor = to_module_expr ftor in
      let arg = to_module_expr arg in
      let res_coercion = to_module_coercion res_coercion in
      Tmod_apply (ftor, arg, res_coercion)
  | Tmod_constraint { mod_expr; mod_type; constraint_; coercion } ->
      let mod_expr = to_module_expr mod_expr in
      let constraint_ = to_module_type_constraint constraint_ in
      let coercion = to_module_coercion coercion in
      Tmod_constraint (mod_expr, mod_type, constraint_, coercion)
  | Tmod_unpack { expr; mod_type } ->
      let expr = to_expression expr in
      Tmod_unpack (expr, mod_type)

and to_structure : structure -> OCaml.structure = fun strc ->
  let str_items = List.map to_structure_item strc.str_items in
  let str_type = strc.str_type in
  let str_final_env = strc.str_final_env in
  { str_items; str_type; str_final_env }

and to_structure_item : structure_item -> OCaml.structure_item = fun strc ->
  let str_desc = to_structure_item_desc strc.str_desc in
  let str_loc = strc.str_loc in
  let str_env = strc.str_env in
  { str_desc; str_loc; str_env }

and to_structure_item_desc : structure_item_desc -> OCaml.structure_item_desc =
  function
  | Tstr_eval { expr; attributes } ->
      let expr = to_expression expr in
      let attributes = to_attributes attributes in
      Tstr_eval (expr, attributes)
  | Tstr_value { rec_; bindings } ->
      let bindings = List.map to_value_binding bindings in
      Tstr_value (rec_, bindings)
  | Tstr_primitive { val_desc } ->
      let val_desc = to_value_description val_desc in
      Tstr_primitive val_desc
  | Tstr_type { rec_; type_decls } ->
      let type_decls = List.map to_type_declaration type_decls in
      Tstr_type (rec_, type_decls)
  | Tstr_typext { type_ext } ->
      let type_ext = to_type_extension type_ext in
      Tstr_typext type_ext
  | Tstr_exception { type_exc } ->
      let type_exc = to_type_exception type_exc in
      Tstr_exception type_exc
  | Tstr_module { mod_binding } ->
      let mod_binding = to_module_binding mod_binding in
      Tstr_module mod_binding
  | Tstr_recmodule { mod_bindings } ->
      let mod_bindings = List.map to_module_binding mod_bindings in
      Tstr_recmodule mod_bindings
  | Tstr_modtype { modtyp_decl } ->
      let modtyp_decl = to_module_type_declaration modtyp_decl in
      Tstr_modtype modtyp_decl
  | Tstr_open { open_decl } ->
      let open_decl = to_open_declaration open_decl in
      Tstr_open open_decl
  | Tstr_class { class_bindings } ->
      let class_bindings =
        let convert (class_decl, meths) =
          let class_decl' = to_class_declaration class_decl in
          (class_decl', meths)
        in
        List.map convert class_bindings
      in
      Tstr_class class_bindings
  | Tstr_class_type { class_types } ->
      let class_types =
        let convert (id, name, clty_decl) =
          let clty_decl' = to_class_type_declaration clty_decl in
          (id, name, clty_decl')
        in
        List.map convert class_types
      in
      Tstr_class_type class_types
  | Tstr_include { incl_decl } ->
      let incl_decl = to_include_declaration incl_decl in
      Tstr_include incl_decl
  | Tstr_attribute { attribute } ->
      let attribute = to_attribute attribute in
      Tstr_attribute attribute

and to_module_binding : module_binding -> OCaml.module_binding = fun mb ->
  let mb_id = mb.mb_id in
  let mb_name = mb.mb_name in
  let mb_presence = mb.mb_presence in
  let mb_expr = to_module_expr mb.mb_expr in
  let mb_attributes = to_attributes mb.mb_attributes in
  let mb_loc = mb.mb_loc in
  { mb_id; mb_name; mb_presence; mb_expr; mb_attributes; mb_loc }

and to_value_binding : value_binding -> OCaml.value_binding = fun vb ->
  let vb_pat = to_pattern vb.vb_pat in
  let vb_expr = to_expression vb.vb_expr in
  let vb_attributes = to_attributes vb.vb_attributes in
  let vb_loc = vb.vb_loc in
  { vb_pat; vb_expr; vb_attributes; vb_loc }

and to_module_coercion : module_coercion -> OCaml.module_coercion = function
  | Tcoerce_none -> Tcoerce_none
  | Tcoerce_structure { pos_coercions; id_pos_list } ->
      let pos_coercions =
        let convert (i, mod_coer) =
          let mod_coer' = to_module_coercion mod_coer in
          (i, mod_coer')
        in
        List.map convert pos_coercions
      in
      let id_pos_list =
        let convert (id, i, mod_coer) =
          let mod_coer' = to_module_coercion mod_coer in
          (id, i, mod_coer')
        in
        List.map convert id_pos_list
      in
      Tcoerce_structure (pos_coercions, id_pos_list)
  | Tcoerce_functor { arg_coercion; res_coercion } ->
      let arg_coercion = to_module_coercion arg_coercion in
      let res_coercion = to_module_coercion res_coercion in
      Tcoerce_functor (arg_coercion, res_coercion)
  | Tcoerce_primitive { coercion } ->
      let coercion = to_primitive_coercion coercion in
      Tcoerce_primitive coercion
  | Tcoerce_alias { env; path; coercion } ->
      let coercion = to_module_coercion coercion in
      Tcoerce_alias (env, path, coercion)

and to_module_type : module_type -> OCaml.module_type = fun mty ->
    let mty_desc = to_module_type_desc mty.mty_desc in
    let mty_type = mty.mty_type  in
    let mty_env = mty.mty_env  in
    let mty_loc = mty.mty_loc in
    let mty_attributes = to_attributes mty.mty_attributes in
    { mty_desc; mty_type; mty_env; mty_loc; mty_attributes }

and to_module_type_desc : module_type_desc -> OCaml.module_type_desc = function
  | Tmty_ident { path; longid } -> Tmty_ident (path, longid)
  | Tmty_signature { sign } ->
      let sign = to_signature sign in
      Tmty_signature sign
  | Tmty_functor { param; res_type } ->
      let param = to_functor_parameter param in
      let res_type = to_module_type res_type in
      Tmty_functor (param, res_type)
  | Tmty_with { mod_type; constraints } ->
      let mod_type = to_module_type mod_type in
      let constraints =
        let convert (path, longid, wc) =
          let wc' = to_with_constraint wc in
          (path, longid, wc')
        in
        List.map convert constraints
      in
      Tmty_with (mod_type, constraints)
  | Tmty_typeof { mod_expr } ->
      let mod_expr = to_module_expr mod_expr in
      Tmty_typeof mod_expr
  | Tmty_alias { path; longid } -> Tmty_alias (path, longid)

and to_primitive_coercion : primitive_coercion -> OCaml.primitive_coercion =
  fun pc ->
  let pc_desc = pc.pc_desc in
  let pc_type = pc.pc_type in
  let pc_env = pc.pc_env in
  let pc_loc = pc.pc_loc in
  { pc_desc; pc_type; pc_env; pc_loc }

and to_signature : signature -> OCaml.signature = fun sign ->
  let sig_items = List.map to_signature_item sign.sig_items in
  let sig_type = sign.sig_type in
  let sig_final_env = sign.sig_final_env in
  { sig_items; sig_type; sig_final_env }

and to_signature_item : signature_item -> OCaml.signature_item = fun sign ->
  let sig_desc = to_signature_item_desc sign.sig_desc in
  let sig_env = sign.sig_env in
  let sig_loc = sign.sig_loc in
  { sig_desc; sig_env; sig_loc }

and to_signature_item_desc : signature_item_desc -> OCaml.signature_item_desc =
  function
  | Tsig_value { val_desc } ->
      let val_desc = to_value_description val_desc in
      Tsig_value val_desc
  | Tsig_type { rec_; type_decls } ->
      let type_decls = List.map to_type_declaration type_decls in
      Tsig_type (rec_, type_decls)
  | Tsig_typesubst { type_decls } ->
      let type_decls = List.map to_type_declaration type_decls in
      Tsig_typesubst type_decls
  | Tsig_typext { type_ext } ->
      let type_ext = to_type_extension type_ext in
      Tsig_typext type_ext
  | Tsig_exception { type_exc } ->
      let type_exc = to_type_exception type_exc in
      Tsig_exception type_exc
  | Tsig_module { mod_decl } ->
      let mod_decl = to_module_declaration mod_decl in
      Tsig_module mod_decl
  | Tsig_modsubst { mod_subst } ->
      let mod_subst = to_module_substitution mod_subst in
      Tsig_modsubst mod_subst
  | Tsig_recmodule { mod_delcs } ->
      let mod_delcs = List.map to_module_declaration mod_delcs in
      Tsig_recmodule mod_delcs
  | Tsig_modtype { modtype_decl } ->
      let modtype_decl = to_module_type_declaration modtype_decl in
      Tsig_modtype modtype_decl
  | Tsig_modtypesubst { modtype_decl } ->
      let modtype_decl = to_module_type_declaration modtype_decl in
      Tsig_modtypesubst modtype_decl
  | Tsig_open { open_desc } ->
      let open_desc = to_open_description open_desc in
      Tsig_open open_desc
  | Tsig_include { incl_desc } ->
      let incl_desc = to_include_description incl_desc in
      Tsig_include incl_desc
  | Tsig_class { class_descs } ->
      let class_descs = List.map to_class_description class_descs in
      Tsig_class class_descs
  | Tsig_class_type { classtype_decls } ->
      let classtype_decls =
        List.map to_class_type_declaration classtype_decls
      in
      Tsig_class_type classtype_decls
  | Tsig_attribute { attribute } ->
      let attribute = to_attribute attribute in
     Tsig_attribute attribute

and to_module_declaration : module_declaration -> OCaml.module_declaration =
  fun md ->
  let md_id = md.md_id in
  let md_name = md.md_name in
  let md_presence = md.md_presence in
  let md_type = to_module_type md.md_type in
  let md_attributes = to_attributes md.md_attributes in
  let md_loc = md.md_loc in
  { md_id; md_name; md_presence; md_type; md_attributes; md_loc }

and to_module_substitution : module_substitution -> OCaml.module_substitution =
  fun ms ->
  let ms_id = ms.ms_id in
  let ms_name = ms.ms_name in
  let ms_manifest = ms.ms_manifest in
  let ms_txt = ms.ms_txt in
  let ms_attributes = to_attributes ms.ms_attributes in
  let ms_loc = ms.ms_loc in
  { ms_id; ms_name; ms_manifest; ms_txt; ms_attributes; ms_loc }

and to_module_type_declaration:
  module_type_declaration -> OCaml.module_type_declaration =
  fun mtd ->
  let mtd_id = mtd.mtd_id in
  let mtd_name = mtd.mtd_name in
  let mtd_type = Option.map to_module_type mtd.mtd_type in
  let mtd_attributes = to_attributes mtd.mtd_attributes in
  let mtd_loc = mtd.mtd_loc in
  { mtd_id; mtd_name; mtd_type; mtd_attributes; mtd_loc }

and to_open_infos:
  type a b . to_open_expr:(a -> b) -> a open_infos -> b OCaml.open_infos =
  fun ~to_open_expr oi ->
  let open_expr = to_open_expr oi.open_expr in
  let open_bound_items = oi.open_bound_items in
  let open_override = oi.open_override in
  let open_env = oi.open_env in
  let open_loc = oi.open_loc in
  let open_attributes = to_attributes oi.open_attributes in
  { open_expr; open_bound_items; open_override; open_env; open_loc; open_attributes }

and to_open_description : open_description -> OCaml.open_description =
  fun open_desc ->
  to_open_infos ~to_open_expr:Fun.id open_desc

and to_open_declaration : open_declaration -> OCaml.open_declaration =
  fun open_decl ->
  to_open_infos ~to_open_expr:to_module_expr open_decl

and to_include_infos:
  type a b . to_incl_mod:(a -> b) -> a include_infos -> b OCaml.include_infos =
  fun ~to_incl_mod ii ->
  let incl_mod = to_incl_mod ii.incl_mod in
  let incl_type = ii.incl_type in
  let incl_loc = ii.incl_loc in
  let incl_attributes = to_attributes ii.incl_attributes in
  { incl_mod; incl_type; incl_loc; incl_attributes }

and to_include_description : include_description -> OCaml.include_description =
  fun incl_desc ->
  to_include_infos ~to_incl_mod:to_module_type incl_desc

and to_include_declaration : include_declaration -> OCaml.include_declaration =
  fun incl_decl ->
  to_include_infos ~to_incl_mod:to_module_expr incl_decl

and to_with_constraint : with_constraint -> OCaml.with_constraint = function
  | Twith_type { type_decl } ->
      let type_decl = to_type_declaration type_decl in
      Twith_type type_decl
  | Twith_module { path; longid } -> Twith_module (path, longid)
  | Twith_modtype { mod_type } ->
      let mod_type = to_module_type mod_type in
      Twith_modtype mod_type
  | Twith_typesubst { type_decl } ->
      let type_decl = to_type_declaration type_decl in
      Twith_typesubst type_decl
  | Twith_modsubst { path; longid } -> Twith_modsubst (path, longid)
  | Twith_modtypesubst { mod_type } ->
      let mod_type = to_module_type mod_type in
      Twith_modtypesubst mod_type

and to_core_type : core_type -> OCaml.core_type = fun ct ->
    let ctyp_desc = to_core_type_desc ct.ctyp_desc in
    let ctyp_type = ct.ctyp_type in
    let ctyp_env = ct.ctyp_env in
    let ctyp_loc = ct.ctyp_loc in
    let ctyp_attributes = to_attributes ct.ctyp_attributes in
    { ctyp_desc; ctyp_type; ctyp_env; ctyp_loc; ctyp_attributes }

and to_core_type_desc : core_type_desc -> OCaml.core_type_desc = function
  | Ttyp_any -> Ttyp_any
  | Ttyp_var { name } -> Ttyp_var name
  | Ttyp_arrow { arg_label; arg_type; res_type } ->
      let arg_type = to_core_type arg_type in
      let res_type = to_core_type res_type in
      Ttyp_arrow (arg_label, arg_type, res_type)
  | Ttyp_tuple { fields } ->
      let fields = List.map to_core_type fields in
      Ttyp_tuple fields
  | Ttyp_constr { path; longid; params } ->
      let params = List.map to_core_type params in
      Ttyp_constr (path, longid, params)
  | Ttyp_object { fields; closed } ->
      let fields = List.map to_object_field fields in
      Ttyp_object (fields, closed)
  | Ttyp_class { path; longid; params } ->
      let params = List.map to_core_type params in
      Ttyp_class (path, longid, params)
  | Ttyp_alias { type_; name } ->
      let type_ = to_core_type type_ in
      Ttyp_alias (type_, name)
  | Ttyp_variant { rows; closed; labels } ->
      let rows = List.map to_row_field rows in
      Ttyp_variant (rows, closed, labels)
  | Ttyp_poly { params; type_ } ->
      let type_ = to_core_type type_ in
      Ttyp_poly (params, type_)
  | Ttyp_package { pack_type } ->
      let pack_type = to_package_type pack_type in
      Ttyp_package (pack_type)

and to_package_type : package_type -> OCaml.package_type = fun pt ->
  let pack_path = pt.pack_path in
  let pack_fields =
    let convert (longid, t) =
      let t' = to_core_type t in
      (longid, t')
    in
    List.map convert pt.pack_fields
  in
  let pack_type = pt.pack_type in
  let pack_txt = pt.pack_txt in
  { pack_path; pack_fields; pack_type; pack_txt }

and to_row_field : row_field -> OCaml.row_field = fun rf ->
  let rf_desc = to_row_field_desc rf.rf_desc in
  let rf_loc = rf.rf_loc in
  let rf_attributes = to_attributes rf.rf_attributes in
  { rf_desc; rf_loc; rf_attributes }

and to_row_field_desc : row_field_desc -> OCaml.row_field_desc = function
  | Ttag { name; empty; conj } ->
      let conj = List.map to_core_type conj in
      Ttag (name, empty, conj)
  | Tinherit { type_ } ->
      let type_ = to_core_type type_ in
      Tinherit type_

and to_object_field : object_field -> OCaml.object_field = fun obf ->
  let of_desc = to_object_field_desc obf.of_desc in
  let of_loc = obf.of_loc in
  let of_attributes = to_attributes obf.of_attributes in
  { of_desc; of_loc; of_attributes }

and to_object_field_desc : object_field_desc -> OCaml.object_field_desc =
  function
  | OTtag { name; type_ } ->
      let type_ = to_core_type type_ in
      OTtag (name, type_)
  | OTinherit { type_ } ->
      let type_ = to_core_type type_ in
      OTinherit type_

and to_value_description : value_description -> OCaml.value_description =
  fun vd ->
  let val_id = vd.val_id in
  let val_name = vd.val_name in
  let val_desc = to_core_type vd.val_desc in
  let val_val = vd.val_val in
  let val_prim = vd.val_prim in
  let val_loc = vd.val_loc in
  let val_attributes = to_attributes vd.val_attributes in
  { val_id; val_name; val_desc; val_val; val_prim; val_loc; val_attributes }

and to_type_declaration : type_declaration -> OCaml.type_declaration = fun td ->
  let typ_id = td.typ_id in
  let typ_name = td.typ_name in
  let typ_params =
    let convert (t, varinj) =
      let t' = to_core_type t in
      (t', varinj)
    in
    List.map convert td.typ_params
  in
  let typ_type = td.typ_type in
  let typ_cstrs =
    let convert (t1, t2, loc) =
      let t1' = to_core_type t1 in
      let t2' = to_core_type t2 in
      (t1', t2', loc)
    in
    List.map convert td.typ_cstrs
  in
  let typ_kind = to_type_kind td.typ_kind in
  let typ_private = td.typ_private in
  let typ_manifest = Option.map to_core_type td.typ_manifest in
  let typ_loc = td.typ_loc in
  let typ_attributes = to_attributes td.typ_attributes in
  { typ_id;
    typ_name;
    typ_params;
    typ_type;
    typ_cstrs;
    typ_kind;
    typ_private;
    typ_manifest;
    typ_loc;
    typ_attributes;
  }

and to_type_kind : type_kind -> OCaml.type_kind = function
  | Ttype_abstract ->Ttype_abstract
  | Ttype_variant { ctor_decls } ->
      let ctor_decls = List.map to_constructor_declaration ctor_decls in
      Ttype_variant ctor_decls
  | Ttype_record { label_decls } ->
      let label_decls = List.map to_label_declaration label_decls in
      Ttype_record label_decls
  | Ttype_open -> Ttype_open

and to_label_declaration : label_declaration -> OCaml.label_declaration =
  fun ld ->
  let ld_id = ld.ld_id in
  let ld_name = ld.ld_name in
  let ld_mutable = ld.ld_mutable in
  let ld_type = to_core_type ld.ld_type in
  let ld_loc = ld.ld_loc in
  let ld_attributes = to_attributes ld.ld_attributes in
  { ld_id; ld_name; ld_mutable; ld_type; ld_loc; ld_attributes }

and to_constructor_declaration :
  constructor_declaration -> OCaml.constructor_declaration =
  fun cd ->
  let cd_id = cd.cd_id in
  let cd_name = cd.cd_name in
  let cd_vars = cd.cd_vars in
  let cd_args = to_constructor_arguments cd.cd_args in
  let cd_res = Option.map to_core_type cd.cd_res in
  let cd_loc = cd.cd_loc in
  let cd_attributes = to_attributes cd.cd_attributes in
  { cd_id; cd_name; cd_vars; cd_args; cd_res; cd_loc; cd_attributes }

and to_constructor_arguments :
  constructor_arguments -> OCaml.constructor_arguments =
  function
  | Cstr_tuple { fields } ->
      let fields = List.map to_core_type fields in
      Cstr_tuple fields
  | Cstr_record { label_decls } ->
      let label_decls = List.map to_label_declaration label_decls in
      Cstr_record label_decls

and to_type_extension : type_extension -> OCaml.type_extension = fun te ->
  let tyext_path = te.tyext_path in
  let tyext_txt = te.tyext_txt in
  let tyext_params =
    let convert (t, varinj) =
      let t' = to_core_type t in
      (t', varinj)
    in
    List.map convert te.tyext_params
  in
  let tyext_constructors =
    List.map to_extension_constructor te.tyext_constructors
  in
  let tyext_private = te.tyext_private in
  let tyext_loc = te.tyext_loc in
  let tyext_attributes = to_attributes te.tyext_attributes in
  { tyext_path;
    tyext_txt;
    tyext_params;
    tyext_constructors;
    tyext_private;
    tyext_loc;
    tyext_attributes;
  }

and to_type_exception : type_exception -> OCaml.type_exception = fun te ->
  let tyexn_constructor = to_extension_constructor te.tyexn_constructor in
  let tyexn_loc = te.tyexn_loc in
  let tyexn_attributes = to_attributes te.tyexn_attributes in
  { tyexn_constructor; tyexn_loc; tyexn_attributes }

and to_extension_constructor :
  extension_constructor -> OCaml.extension_constructor =
  fun ec ->
  let ext_id = ec.ext_id in
  let ext_name = ec.ext_name in
  let ext_type = ec.ext_type in
  let ext_kind = to_extension_constructor_kind ec.ext_kind in
  let ext_loc = ec.ext_loc in
  let ext_attributes = to_attributes ec.ext_attributes in
  { ext_id; ext_name; ext_type; ext_kind; ext_loc; ext_attributes }

and to_extension_constructor_kind :
  extension_constructor_kind -> OCaml.extension_constructor_kind =
  function
  | Text_decl { existentials; arg; res_type } ->
      let arg = to_constructor_arguments arg in
      let res_type = Option.map to_core_type res_type in
      Text_decl (existentials, arg, res_type)
  | Text_rebind { path; longid } -> Text_rebind (path, longid)

and to_class_type : class_type -> OCaml.class_type = fun ct ->
  let cltyp_desc = to_class_type_desc ct.cltyp_desc in
  let cltyp_type = ct.cltyp_type in
  let cltyp_env = ct.cltyp_env in
  let cltyp_loc = ct.cltyp_loc in
  let cltyp_attributes = to_attributes ct.cltyp_attributes in
  { cltyp_desc; cltyp_type; cltyp_env; cltyp_loc; cltyp_attributes }

and to_class_type_desc : class_type_desc -> OCaml.class_type_desc = function
  | Tcty_constr { path; longid; params } ->
      let params = List.map to_core_type params in
      Tcty_constr (path, longid, params)
  | Tcty_signature { class_sign } ->
      let class_sign = to_class_signature class_sign in
      Tcty_signature class_sign
  | Tcty_arrow { arg_label; arg_type; class_type } ->
      let arg_type = to_core_type arg_type in
      let class_type = to_class_type class_type in
      Tcty_arrow (arg_label, arg_type, class_type)
  | Tcty_open { open_desc; class_type } ->
      let open_desc = to_open_description open_desc in
      let class_type = to_class_type class_type in
      Tcty_open (open_desc, class_type)

and to_class_signature : class_signature -> OCaml.class_signature = fun cs ->
  let csig_self = to_core_type cs.csig_self in
  let csig_fields = List.map to_class_type_field cs.csig_fields in
  let csig_type = cs.csig_type in
  { csig_self; csig_fields; csig_type }

and to_class_type_field : class_type_field -> OCaml.class_type_field = fun cf ->
  let ctf_desc = to_class_type_field_desc cf.ctf_desc in
  let ctf_loc = cf.ctf_loc in
  let ctf_attributes = to_attributes cf.ctf_attributes in
  { ctf_desc; ctf_loc; ctf_attributes }

and to_class_type_field_desc :
  class_type_field_desc -> OCaml.class_type_field_desc =
  function
  | Tctf_inherit { parent_type } ->
      let parent_type = to_class_type parent_type in
      Tctf_inherit parent_type
  | Tctf_val { name; mut; virt; type_ } ->
      let type_ = to_core_type type_ in
      Tctf_val (name, mut, virt, type_)
  | Tctf_method { name; priv; virt; type_ } ->
      let type_ = to_core_type type_ in
      Tctf_method (name, priv, virt, type_)
  | Tctf_constraint { type1; type2 } ->
      let type1 = to_core_type type1 in
      let type2 = to_core_type type2 in
      Tctf_constraint (type1, type2)
  | Tctf_attribute { attribute } ->
      let attribute = to_attribute attribute in
      Tctf_attribute attribute

and to_class_declaration : class_declaration -> OCaml.class_declaration =
  fun class_decl ->
  to_class_infos ~to_ci_expr:to_class_expr class_decl

and to_class_description : class_description -> OCaml.class_description =
  fun class_desc ->
  to_class_infos ~to_ci_expr:to_class_type class_desc

and to_class_type_declaration : class_type_declaration -> OCaml.class_type_declaration =
  fun clty_decl ->
  to_class_infos ~to_ci_expr:to_class_type clty_decl

and to_class_infos :
  type a b . to_ci_expr:(a -> b) -> a class_infos -> b OCaml.class_infos =
  fun ~to_ci_expr ci ->
  let ci_virt = ci.ci_virt in
  let ci_params =
    let convert (t, varinj) =
      let t' = to_core_type t in
      (t', varinj)
    in
   List.map convert ci.ci_params
  in
  let ci_id_name = ci.ci_id_name in
  let ci_id_class = ci.ci_id_class in
  let ci_id_class_type = ci.ci_id_class_type in
  let ci_id_object = ci.ci_id_object in
  let ci_id_typehash = ci.ci_id_typehash in
  let ci_expr = to_ci_expr ci.ci_expr in
  let ci_decl = ci.ci_decl in
  let ci_type_decl = ci.ci_type_decl in
  let ci_loc = ci.ci_loc in
  let ci_attributes = to_attributes ci.ci_attributes in
  { ci_virt;
    ci_params;
    ci_id_name;
    ci_id_class;
    ci_id_class_type;
    ci_id_object;
    ci_id_typehash;
    ci_expr;
    ci_decl;
    ci_type_decl;
    ci_loc;
    ci_attributes;
  }
