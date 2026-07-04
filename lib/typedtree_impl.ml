include Typedtree_intf

module OCaml = OCaml.Typedtree

let of_partial : OCaml.partial -> partial = failwith "Not implemented"
let to_partial : partial -> OCaml.partial = failwith "Not implemented"

let of_attribute : OCaml.attribute -> attribute = failwith "Not implemented"
let to_attribute : attribute -> OCaml.attribute = failwith "Not implemented"

let of_attributes : OCaml.attributes -> attributes = failwith "Not implemented"
let to_attributes : attributes -> OCaml.attributes = failwith "Not implemented"

let of_value : OCaml.value -> value = failwith "Not implemented"
let to_value : value -> OCaml.value = failwith "Not implemented"

let of_computation : OCaml.computation -> computation = failwith "Not implemented"
let to_computation : computation -> OCaml.computation = failwith "Not implemented"

let of_pattern : OCaml.pattern -> pattern = failwith "Not implemented"
let to_pattern : pattern -> OCaml.pattern = failwith "Not implemented"

let of_general_pattern : 'k . 'k OCaml.general_pattern -> 'k general_pattern = function _ -> failwith "Not implemented"
let to_general_pattern : 'k . 'k general_pattern -> 'k OCaml.general_pattern = function _ -> failwith "Not implemented"

let of_pattern_data : 'a . 'a OCaml.pattern_data -> 'a pattern_data = function _ -> failwith "Not implemented"
let to_pattern_data : 'a pattern_data -> 'a OCaml.pattern_data = function _ -> failwith "Not implemented"

let of_pat_extra : OCaml.pat_extra -> pat_extra = failwith "Not implemented"
let to_pat_extra : pat_extra -> OCaml.pat_extra = failwith "Not implemented"

let of_pattern_desc : 'k . 'k OCaml.pattern_desc -> 'k pattern_desc = function _ -> failwith "Not implemented"
let to_pattern_desc : 'k . 'k pattern_desc -> 'k OCaml.pattern_desc = function _ -> failwith "Not implemented"

let of_tpat_value_argument : OCaml.tpat_value_argument -> tpat_value_argument = failwith "Not implemented"
let to_tpat_value_argument : tpat_value_argument -> OCaml.tpat_value_argument = failwith "Not implemented"

let of_expression : OCaml.expression -> expression = failwith "Not implemented"
let to_expression : expression -> OCaml.expression = failwith "Not implemented"

let of_exp_extra : OCaml.exp_extra -> exp_extra = failwith "Not implemented"
let to_exp_extra : exp_extra -> OCaml.exp_extra = failwith "Not implemented"

let of_expression_desc : OCaml.expression_desc -> expression_desc = failwith "Not implemented"
let to_expression_desc : expression_desc -> OCaml.expression_desc = failwith "Not implemented"

let of_meth : OCaml.meth -> meth = failwith "Not implemented"
let to_meth : meth -> OCaml.meth = failwith "Not implemented"

let of_case : 'k . 'k OCaml.case -> 'k case = function _ -> failwith "Not implemented"
let to_case : 'k . 'k case -> 'k OCaml.case = function _ -> failwith "Not implemented"

let of_record_label_definition : OCaml.record_label_definition -> record_label_definition = failwith "Not implemented"
let to_record_label_definition : record_label_definition -> OCaml.record_label_definition = failwith "Not implemented"

let of_binding_op : OCaml.binding_op -> binding_op = failwith "Not implemented"
let to_binding_op : binding_op -> OCaml.binding_op = failwith "Not implemented"

let of_class_expr : OCaml.class_expr -> class_expr = failwith "Not implemented"
let to_class_expr : class_expr -> OCaml.class_expr = failwith "Not implemented"

let of_class_expr_desc : OCaml.class_expr_desc -> class_expr_desc = failwith "Not implemented"
let to_class_expr_desc : class_expr_desc -> OCaml.class_expr_desc = failwith "Not implemented"

let of_class_structure : OCaml.class_structure -> class_structure = failwith "Not implemented"
let to_class_structure : class_structure -> OCaml.class_structure = failwith "Not implemented"

let of_class_field : OCaml.class_field -> class_field = failwith "Not implemented"
let to_class_field : class_field -> OCaml.class_field = failwith "Not implemented"

let of_class_field_kind : OCaml.class_field_kind -> class_field_kind = failwith "Not implemented"
let to_class_field_kind : class_field_kind -> OCaml.class_field_kind = failwith "Not implemented"

let of_class_field_desc : OCaml.class_field_desc -> class_field_desc = failwith "Not implemented"
let to_class_field_desc : class_field_desc -> OCaml.class_field_desc = failwith "Not implemented"

let of_module_expr : OCaml.module_expr -> module_expr = failwith "Not implemented"
let to_module_expr : module_expr -> OCaml.module_expr = failwith "Not implemented"

let of_module_type_constraint : OCaml.module_type_constraint -> module_type_constraint = failwith "Not implemented"
let to_module_type_constraint : module_type_constraint -> OCaml.module_type_constraint = failwith "Not implemented"

let of_functor_parameter : OCaml.functor_parameter -> functor_parameter = failwith "Not implemented"
let to_functor_parameter : functor_parameter -> OCaml.functor_parameter = failwith "Not implemented"

let of_module_expr_desc : OCaml.module_expr_desc -> module_expr_desc = failwith "Not implemented"
let to_module_expr_desc : module_expr_desc -> OCaml.module_expr_desc = failwith "Not implemented"

let of_structure : OCaml.structure -> structure = failwith "Not implemented"
let to_structure : structure -> OCaml.structure = failwith "Not implemented"

let of_structure_item : OCaml.structure_item -> structure_item = failwith "Not implemented"
let to_structure_item : structure_item -> OCaml.structure_item = failwith "Not implemented"

let of_structure_item_desc : OCaml.structure_item_desc -> structure_item_desc = failwith "Not implemented"
let to_structure_item_desc : structure_item_desc -> OCaml.structure_item_desc = failwith "Not implemented"

let of_module_binding : OCaml.module_binding -> module_binding = failwith "Not implemented"
let to_module_binding : module_binding -> OCaml.module_binding = failwith "Not implemented"

let of_value_binding : OCaml.value_binding -> value_binding = failwith "Not implemented"
let to_value_binding : value_binding -> OCaml.value_binding = failwith "Not implemented"

let of_module_coercion : OCaml.module_coercion -> module_coercion = failwith "Not implemented"
let to_module_coercion : module_coercion -> OCaml.module_coercion = failwith "Not implemented"

let of_module_type : OCaml.module_type -> module_type = failwith "Not implemented"
let to_module_type : module_type -> OCaml.module_type = failwith "Not implemented"

let of_module_type_desc : OCaml.module_type_desc -> module_type_desc = failwith "Not implemented"
let to_module_type_desc : module_type_desc -> OCaml.module_type_desc = failwith "Not implemented"

let of_primitive_coercion : OCaml.primitive_coercion -> primitive_coercion = failwith "Not implemented"
let to_primitive_coercion : primitive_coercion -> OCaml.primitive_coercion = failwith "Not implemented"

let of_signature : OCaml.signature -> signature = failwith "Not implemented"
let to_signature : signature -> OCaml.signature = failwith "Not implemented"

let of_signature_item : OCaml.signature_item -> signature_item = failwith "Not implemented"
let to_signature_item : signature_item -> OCaml.signature_item = failwith "Not implemented"

let of_signature_item_desc : OCaml.signature_item_desc -> signature_item_desc = failwith "Not implemented"
let to_signature_item_desc : signature_item_desc -> OCaml.signature_item_desc = failwith "Not implemented"

let of_module_declaration : OCaml.module_declaration -> module_declaration = failwith "Not implemented"
let to_module_declaration : module_declaration -> OCaml.module_declaration = failwith "Not implemented"

let of_module_substitution : OCaml.module_substitution -> module_substitution = failwith "Not implemented"
let to_module_substitution : module_substitution -> OCaml.module_substitution = failwith "Not implemented"

let of_module_type_declaration : OCaml.module_type_declaration -> module_type_declaration = failwith "Not implemented"
let to_module_type_declaration : module_type_declaration -> OCaml.module_type_declaration = failwith "Not implemented"

let of_open_infos : 'a . 'a OCaml.open_infos -> 'a open_infos = function _ -> failwith "Not implemented"
let to_open_infos : 'a . 'a open_infos -> 'a OCaml.open_infos = function _ -> failwith "Not implemented"

let of_open_description : OCaml.open_description -> open_description = failwith "Not implemented"
let to_open_description : open_description -> OCaml.open_description = failwith "Not implemented"

let of_open_declaration : OCaml.open_declaration -> open_declaration = failwith "Not implemented"
let to_open_declaration : open_declaration -> OCaml.open_declaration = failwith "Not implemented"

let of_include_infos : 'a . 'a OCaml.include_infos -> 'a include_infos = function _ -> failwith "Not implemented"
let to_include_infos : 'a . 'a include_infos -> 'a OCaml.include_infos = function _ -> failwith "Not implemented"

let of_include_description : OCaml.include_description -> include_description = failwith "Not implemented"
let to_include_description : include_description -> OCaml.include_description = failwith "Not implemented"

let of_include_declaration : OCaml.include_declaration -> include_declaration = failwith "Not implemented"
let to_include_declaration : include_declaration -> OCaml.include_declaration = failwith "Not implemented"

let of_with_constraint : OCaml.with_constraint -> with_constraint = failwith "Not implemented"
let to_with_constraint : with_constraint -> OCaml.with_constraint = failwith "Not implemented"

let of_core_type : OCaml.core_type -> core_type = failwith "Not implemented"
let to_core_type : core_type -> OCaml.core_type = failwith "Not implemented"

let of_core_type_desc : OCaml.core_type_desc -> core_type_desc = failwith "Not implemented"
let to_core_type_desc : core_type_desc -> OCaml.core_type_desc = failwith "Not implemented"

let of_package_type : OCaml.package_type -> package_type = failwith "Not implemented"
let to_package_type : package_type -> OCaml.package_type = failwith "Not implemented"

let of_row_field : OCaml.row_field -> row_field = failwith "Not implemented"
let to_row_field : row_field -> OCaml.row_field = failwith "Not implemented"

let of_row_field_desc : OCaml.row_field_desc -> row_field_desc = failwith "Not implemented"
let to_row_field_desc : row_field_desc -> OCaml.row_field_desc = failwith "Not implemented"

let of_object_field : OCaml.object_field -> object_field = failwith "Not implemented"
let to_object_field : object_field -> OCaml.object_field = failwith "Not implemented"

let of_object_field_desc : OCaml.object_field_desc -> object_field_desc = failwith "Not implemented"
let to_object_field_desc : object_field_desc -> OCaml.object_field_desc = failwith "Not implemented"

let of_value_description : OCaml.value_description -> value_description = failwith "Not implemented"
let to_value_description : value_description -> OCaml.value_description = failwith "Not implemented"

let of_type_declaration : OCaml.type_declaration -> type_declaration = failwith "Not implemented"
let to_type_declaration : type_declaration -> OCaml.type_declaration = failwith "Not implemented"

let of_type_kind : OCaml.type_kind -> type_kind = failwith "Not implemented"
let to_type_kind : type_kind -> OCaml.type_kind = failwith "Not implemented"

let of_label_declaration : OCaml.label_declaration -> label_declaration = failwith "Not implemented"
let to_label_declaration : label_declaration -> OCaml.label_declaration = failwith "Not implemented"

let of_constructor_declaration : OCaml.constructor_declaration -> constructor_declaration = failwith "Not implemented"
let to_constructor_declaration : constructor_declaration -> OCaml.constructor_declaration = failwith "Not implemented"

let of_constructor_arguments : OCaml.constructor_arguments -> constructor_arguments = failwith "Not implemented"
let to_constructor_arguments : constructor_arguments -> OCaml.constructor_arguments = failwith "Not implemented"

let of_type_extension : OCaml.type_extension -> type_extension = failwith "Not implemented"
let to_type_extension : type_extension -> OCaml.type_extension = failwith "Not implemented"

let of_type_exception : OCaml.type_exception -> type_exception = failwith "Not implemented"
let to_type_exception : type_exception -> OCaml.type_exception = failwith "Not implemented"

let of_extension_constructor : OCaml.extension_constructor -> extension_constructor = failwith "Not implemented"
let to_extension_constructor : extension_constructor -> OCaml.extension_constructor = failwith "Not implemented"

let of_extension_constructor_kind : OCaml.extension_constructor_kind -> extension_constructor_kind = failwith "Not implemented"
let to_extension_constructor_kind : extension_constructor_kind -> OCaml.extension_constructor_kind = failwith "Not implemented"

let of_class_type : OCaml.class_type -> class_type = failwith "Not implemented"
let to_class_type : class_type -> OCaml.class_type = failwith "Not implemented"

let of_class_type_desc : OCaml.class_type_desc -> class_type_desc = failwith "Not implemented"
let to_class_type_desc : class_type_desc -> OCaml.class_type_desc = failwith "Not implemented"

let of_class_signature : OCaml.class_signature -> class_signature = failwith "Not implemented"
let to_class_signature : class_signature -> OCaml.class_signature = failwith "Not implemented"

let of_class_type_field : OCaml.class_type_field -> class_type_field = failwith "Not implemented"
let to_class_type_field : class_type_field -> OCaml.class_type_field = failwith "Not implemented"

let of_class_type_field_desc : OCaml.class_type_field_desc -> class_type_field_desc = failwith "Not implemented"
let to_class_type_field_desc : class_type_field_desc -> OCaml.class_type_field_desc = failwith "Not implemented"

let of_class_declaration : OCaml.class_declaration -> class_declaration = failwith "Not implemented"
let to_class_declaration : class_declaration -> OCaml.class_declaration = failwith "Not implemented"

let of_class_description : OCaml.class_description -> class_description = failwith "Not implemented"
let to_class_description : class_description -> OCaml.class_description = failwith "Not implemented"

let of_class_type_declaration : OCaml.class_type_declaration -> class_type_declaration = failwith "Not implemented"
let to_class_type_declaration : class_type_declaration -> OCaml.class_type_declaration = failwith "Not implemented"

let of_class_infos : 'a . 'a OCaml.class_infos -> 'a class_infos = function _ -> failwith "Not implemented"
let to_class_infos : 'a . 'a class_infos -> 'a OCaml.class_infos = function _ -> failwith "Not implemented"
