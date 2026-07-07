include module type of Typedtree_intf (** @inline *)

module OCaml = OCaml.Typedtree (** compiler-lib's Typedtree *)

val of_partial : OCaml.partial -> partial
val to_partial : partial -> OCaml.partial

val of_attribute : OCaml.attribute -> attribute
val to_attribute : attribute -> OCaml.attribute

val of_attributes : OCaml.attributes -> attributes
val to_attributes : attributes -> OCaml.attributes

val of_value : OCaml.value -> value
val to_value : value -> OCaml.value

val of_computation : OCaml.computation -> computation
val to_computation : computation -> OCaml.computation

val of_pattern : OCaml.pattern -> pattern
val to_pattern : pattern -> OCaml.pattern

val of_general_pattern : 'k . 'k OCaml.general_pattern -> 'k general_pattern
val to_general_pattern : 'k . 'k general_pattern -> 'k OCaml.general_pattern

val of_pattern_data :
  of_pat_desc:('a -> 'b) -> 'a OCaml.pattern_data -> 'b pattern_data
val to_pattern_data :
  to_pat_desc:('b -> 'a) -> 'b pattern_data -> 'a OCaml.pattern_data

val of_pat_extra : OCaml.pat_extra -> pat_extra
val to_pat_extra : pat_extra -> OCaml.pat_extra

val of_pattern_desc : 'k OCaml.pattern_desc -> 'k pattern_desc
val to_pattern_desc : 'k pattern_desc -> 'k OCaml.pattern_desc

val of_tpat_value_argument : OCaml.tpat_value_argument -> tpat_value_argument
val to_tpat_value_argument : tpat_value_argument -> OCaml.tpat_value_argument

val of_expression : OCaml.expression -> expression
val to_expression : expression -> OCaml.expression

val of_exp_extra : OCaml.exp_extra -> exp_extra
val to_exp_extra : exp_extra -> OCaml.exp_extra

val of_expression_desc : OCaml.expression_desc -> expression_desc
val to_expression_desc : expression_desc -> OCaml.expression_desc

val of_meth : OCaml.meth -> meth
val to_meth : meth -> OCaml.meth

val of_case : 'k OCaml.case -> 'k case
val to_case : 'k case -> 'k OCaml.case

val of_record_label_definition : OCaml.record_label_definition -> record_label_definition
val to_record_label_definition : record_label_definition -> OCaml.record_label_definition

val of_binding_op : OCaml.binding_op -> binding_op
val to_binding_op : binding_op -> OCaml.binding_op

val of_class_expr : OCaml.class_expr -> class_expr
val to_class_expr : class_expr -> OCaml.class_expr

val of_class_expr_desc : OCaml.class_expr_desc -> class_expr_desc
val to_class_expr_desc : class_expr_desc -> OCaml.class_expr_desc

val of_class_structure : OCaml.class_structure -> class_structure
val to_class_structure : class_structure -> OCaml.class_structure

val of_class_field : OCaml.class_field -> class_field
val to_class_field : class_field -> OCaml.class_field

val of_class_field_kind : OCaml.class_field_kind -> class_field_kind
val to_class_field_kind : class_field_kind -> OCaml.class_field_kind

val of_class_field_desc : OCaml.class_field_desc -> class_field_desc
val to_class_field_desc : class_field_desc -> OCaml.class_field_desc

val of_module_expr : OCaml.module_expr -> module_expr
val to_module_expr : module_expr -> OCaml.module_expr

val of_module_type_constraint : OCaml.module_type_constraint -> module_type_constraint
val to_module_type_constraint : module_type_constraint -> OCaml.module_type_constraint

val of_functor_parameter : OCaml.functor_parameter -> functor_parameter
val to_functor_parameter : functor_parameter -> OCaml.functor_parameter

val of_module_expr_desc : OCaml.module_expr_desc -> module_expr_desc
val to_module_expr_desc : module_expr_desc -> OCaml.module_expr_desc

val of_structure : OCaml.structure -> structure
val to_structure : structure -> OCaml.structure

val of_structure_item : OCaml.structure_item -> structure_item
val to_structure_item : structure_item -> OCaml.structure_item

val of_structure_item_desc : OCaml.structure_item_desc -> structure_item_desc
val to_structure_item_desc : structure_item_desc -> OCaml.structure_item_desc

val of_module_binding : OCaml.module_binding -> module_binding
val to_module_binding : module_binding -> OCaml.module_binding

val of_value_binding : OCaml.value_binding -> value_binding
val to_value_binding : value_binding -> OCaml.value_binding

val of_module_coercion : OCaml.module_coercion -> module_coercion
val to_module_coercion : module_coercion -> OCaml.module_coercion

val of_module_type : OCaml.module_type -> module_type
val to_module_type : module_type -> OCaml.module_type

val of_module_type_desc : OCaml.module_type_desc -> module_type_desc
val to_module_type_desc : module_type_desc -> OCaml.module_type_desc

val of_primitive_coercion : OCaml.primitive_coercion -> primitive_coercion
val to_primitive_coercion : primitive_coercion -> OCaml.primitive_coercion

val of_signature : OCaml.signature -> signature
val to_signature : signature -> OCaml.signature

val of_signature_item : OCaml.signature_item -> signature_item
val to_signature_item : signature_item -> OCaml.signature_item

val of_signature_item_desc : OCaml.signature_item_desc -> signature_item_desc
val to_signature_item_desc : signature_item_desc -> OCaml.signature_item_desc

val of_module_declaration : OCaml.module_declaration -> module_declaration
val to_module_declaration : module_declaration -> OCaml.module_declaration

val of_module_substitution : OCaml.module_substitution -> module_substitution
val to_module_substitution : module_substitution -> OCaml.module_substitution

val of_module_type_declaration : OCaml.module_type_declaration -> module_type_declaration
val to_module_type_declaration : module_type_declaration -> OCaml.module_type_declaration

val of_open_infos :
  of_open_expr:('a -> 'b) -> 'a OCaml.open_infos -> 'b open_infos
val to_open_infos :
  to_open_expr:('b -> 'a) -> 'b open_infos -> 'a OCaml.open_infos

val of_open_description : OCaml.open_description -> open_description
val to_open_description : open_description -> OCaml.open_description

val of_open_declaration : OCaml.open_declaration -> open_declaration
val to_open_declaration : open_declaration -> OCaml.open_declaration

val of_include_infos :
  of_incl_mod:('a -> 'b) -> 'a OCaml.include_infos -> 'b include_infos
val to_include_infos :
  to_incl_mod:('b -> 'a) -> 'b include_infos -> 'a OCaml.include_infos

val of_include_description : OCaml.include_description -> include_description
val to_include_description : include_description -> OCaml.include_description

val of_include_declaration : OCaml.include_declaration -> include_declaration
val to_include_declaration : include_declaration -> OCaml.include_declaration

val of_with_constraint : OCaml.with_constraint -> with_constraint
val to_with_constraint : with_constraint -> OCaml.with_constraint

val of_core_type : OCaml.core_type -> core_type
val to_core_type : core_type -> OCaml.core_type

val of_core_type_desc : OCaml.core_type_desc -> core_type_desc
val to_core_type_desc : core_type_desc -> OCaml.core_type_desc

val of_package_type : OCaml.package_type -> package_type
val to_package_type : package_type -> OCaml.package_type

val of_row_field : OCaml.row_field -> row_field
val to_row_field : row_field -> OCaml.row_field

val of_row_field_desc : OCaml.row_field_desc -> row_field_desc
val to_row_field_desc : row_field_desc -> OCaml.row_field_desc

val of_object_field : OCaml.object_field -> object_field
val to_object_field : object_field -> OCaml.object_field

val of_object_field_desc : OCaml.object_field_desc -> object_field_desc
val to_object_field_desc : object_field_desc -> OCaml.object_field_desc

val of_value_description : OCaml.value_description -> value_description
val to_value_description : value_description -> OCaml.value_description

val of_type_declaration : OCaml.type_declaration -> type_declaration
val to_type_declaration : type_declaration -> OCaml.type_declaration

val of_type_kind : OCaml.type_kind -> type_kind
val to_type_kind : type_kind -> OCaml.type_kind

val of_label_declaration : OCaml.label_declaration -> label_declaration
val to_label_declaration : label_declaration -> OCaml.label_declaration

val of_constructor_declaration : OCaml.constructor_declaration -> constructor_declaration
val to_constructor_declaration : constructor_declaration -> OCaml.constructor_declaration

val of_constructor_arguments : OCaml.constructor_arguments -> constructor_arguments
val to_constructor_arguments : constructor_arguments -> OCaml.constructor_arguments

val of_type_extension : OCaml.type_extension -> type_extension
val to_type_extension : type_extension -> OCaml.type_extension

val of_type_exception : OCaml.type_exception -> type_exception
val to_type_exception : type_exception -> OCaml.type_exception

val of_extension_constructor : OCaml.extension_constructor -> extension_constructor
val to_extension_constructor : extension_constructor -> OCaml.extension_constructor

val of_extension_constructor_kind : OCaml.extension_constructor_kind -> extension_constructor_kind
val to_extension_constructor_kind : extension_constructor_kind -> OCaml.extension_constructor_kind

val of_class_type : OCaml.class_type -> class_type
val to_class_type : class_type -> OCaml.class_type

val of_class_type_desc : OCaml.class_type_desc -> class_type_desc
val to_class_type_desc : class_type_desc -> OCaml.class_type_desc

val of_class_signature : OCaml.class_signature -> class_signature
val to_class_signature : class_signature -> OCaml.class_signature

val of_class_type_field : OCaml.class_type_field -> class_type_field
val to_class_type_field : class_type_field -> OCaml.class_type_field

val of_class_type_field_desc : OCaml.class_type_field_desc -> class_type_field_desc
val to_class_type_field_desc : class_type_field_desc -> OCaml.class_type_field_desc

val of_class_declaration : OCaml.class_declaration -> class_declaration
val to_class_declaration : class_declaration -> OCaml.class_declaration

val of_class_description : OCaml.class_description -> class_description
val to_class_description : class_description -> OCaml.class_description

val of_class_type_declaration : OCaml.class_type_declaration -> class_type_declaration
val to_class_type_declaration : class_type_declaration -> OCaml.class_type_declaration

val of_class_infos :
  of_ci_expr:('a -> 'b) -> 'a OCaml.class_infos -> 'b class_infos
val to_class_infos :
  to_ci_expr:('b -> 'a) -> 'b class_infos -> 'a OCaml.class_infos
