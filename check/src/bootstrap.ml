type stats = { mutable pass : int; mutable fail : int }

let print_stats { pass; fail } =
  let total = pass + fail in
  print_endline ("Pass: " ^ string_of_int pass);
  print_endline ("Fail: " ^ string_of_int fail);
  print_endline ("Total: " ^ string_of_int total)

let bootstrap_mapper : Tast_mapper.mapper =
  (* rCconvert back and forth each node before traversing the result *)
  let open Vaast.Typedtree in
  let default = Tast_mapper.default in
  {
    #if OCAML_VERSION < (5, 1, 0)
    #elif OCAML_VERSION >= (5, 1, 0)
    attribute = (fun self x ->
        of_attribute x
        |> to_attribute
        |> default.attribute self);
    attributes = (fun self x ->
        of_attributes x
        |> to_attributes
        |> default.attributes self);
    #endif
    binding_op = (fun self x ->
        of_binding_op x
        |> to_binding_op
        |> default.binding_op self);
    case = (fun self x ->
        of_case x
        |> to_case
        |> default.case self);
    class_declaration = (fun self x ->
        of_class_declaration x
        |> to_class_declaration
        |> default.class_declaration self);
    class_description = (fun self x ->
        of_class_description x
        |> to_class_description
        |> default.class_description self);
    class_expr = (fun self x ->
        of_class_expr x
        |> to_class_expr
        |> default.class_expr self);
    class_field = (fun self x ->
        of_class_field x
        |> to_class_field
        |> default.class_field self);
    class_signature = (fun self x ->
        of_class_signature x
        |> to_class_signature
        |> default.class_signature self);
    class_structure = (fun self x ->
        of_class_structure x
        |> to_class_structure
        |> default.class_structure self);
    class_type = (fun self x ->
        of_class_type x
        |> to_class_type
        |> default.class_type self);
    class_type_declaration = (fun self x ->
        of_class_type_declaration x
        |> to_class_type_declaration
        |> default.class_type_declaration self);
    class_type_field = (fun self x ->
        of_class_type_field x
        |> to_class_type_field
        |> default.class_type_field self);
    env = default.env;
    expr = (fun self x ->
        of_expression x
        |> to_expression
        |> default.expr self);
    extension_constructor = (fun self x ->
        of_extension_constructor x
        |> to_extension_constructor
        |> default.extension_constructor self);
    #if OCAML_VERSION < (5, 1, 0)
    #elif OCAML_VERSION >= (5, 1, 0)
    location = default.location;
    #endif
    module_binding = (fun self x ->
        of_module_binding x
        |> to_module_binding
        |> default.module_binding self);
    module_coercion = (fun self x ->
        of_module_coercion x
        |> to_module_coercion
        |> default.module_coercion self);
    module_declaration = (fun self x ->
        of_module_declaration x
        |> to_module_declaration
        |> default.module_declaration self);
    module_substitution = (fun self x ->
        of_module_substitution x
        |> to_module_substitution
        |> default.module_substitution self);
    module_expr = (fun self x ->
        of_module_expr x
        |> to_module_expr
        |> default.module_expr self);
    module_type = (fun self x ->
        of_module_type x
        |> to_module_type
        |> default.module_type self);
    module_type_declaration = (fun self x ->
        of_module_type_declaration x
        |> to_module_type_declaration
        |> default.module_type_declaration self);
    package_type = (fun self x ->
        of_package_type x
        |> to_package_type
        |> default.package_type self);
    pat = (fun self x ->
        of_general_pattern x
        |> to_general_pattern
        |> default.pat self);
    row_field = (fun self x ->
        of_row_field x
        |> to_row_field
        |> default.row_field self);
    object_field = (fun self x ->
        of_object_field x
        |> to_object_field
        |> default.object_field self);
    open_declaration = (fun self x ->
        of_open_declaration x
        |> to_open_declaration
        |> default.open_declaration self);
    open_description = (fun self x ->
        of_open_description x
        |> to_open_description
        |> default.open_description self);
    signature = (fun self x ->
        of_signature x
        |> to_signature
        |> default.signature self);
    signature_item = (fun self x ->
        of_signature_item x
        |> to_signature_item
        |> default.signature_item self);
    structure = (fun self x ->
        of_structure x
        |> to_structure
        |> default.structure self);
    structure_item = (fun self x ->
        of_structure_item x
        |> to_structure_item
        |> default.structure_item self);
    typ = (fun self x ->
        of_core_type x
        |> to_core_type
        |> default.typ self);
    type_declaration = (fun self x ->
        of_type_declaration x
        |> to_type_declaration
        |> default.type_declaration self);
    type_declarations = (fun self (rec_, tds) ->
        let tds =
          List.map (fun x -> of_type_declaration x |> to_type_declaration) tds
        in
        default.type_declarations self (rec_, tds));
    type_extension = (fun self x ->
        of_type_extension x
        |> to_type_extension
        |> default.type_extension self);
    type_exception = (fun self x ->
        of_type_exception x
        |> to_type_exception
        |> default.type_exception self);
    type_kind = (fun self x ->
        of_type_kind x
        |> to_type_kind
        |> default.type_kind self);
    value_binding = (fun self x ->
        of_value_binding x
        |> to_value_binding
        |> default.value_binding self);
    value_bindings = (fun self (rec_, vbs) ->
        let vbs =
          List.map (fun x -> of_value_binding x |> to_value_binding) vbs
        in
        default.value_bindings self (rec_, vbs));
    value_description = (fun self x ->
        of_value_description x
        |> to_value_description
        |> default.value_description self);
    with_constraint = (fun self x ->
        of_with_constraint x
        |> to_with_constraint
        |> default.with_constraint self);
  }

let bootstrap_cmt filename =
  let bootstrap copy pp original =
    let copy = copy original in
    assert (copy != original);
    (* TODO: write comparison functions for OCaml.Typedtree (and
       Vaast.Typedtree). Relying on the "string"-representation of
       OCaml.Typedtree does not provide all the details and is only temporary.
    *)
    let str_of pp x =
      pp Format.str_formatter x;
      Format.flush_str_formatter ()
    in
    let expected = str_of pp original in
    let got = str_of pp copy in
    if String.equal expected got then Result.Ok ()
    else
      let error =
        "Results differ:\n"
        ^ String.concat "\n```" ["expected:"; expected; "\ngot"; got; ""]
      in
      Result.Error error
  in
  let error_annot s =
    Result.Error ("Expected a complete Interface or Implementation. Got a " ^ s)
  in
  try
    let cmt_infos = Cmt_format.read_cmt filename in
    let module VT = Vaast.Typedtree in
    match cmt_infos.cmt_annots with
    | Implementation structure ->
        let pp = Printtyped.implementation in
        let copy = bootstrap_mapper.structure bootstrap_mapper in
        bootstrap copy pp structure
    | Interface signature ->
        let pp = Printtyped.interface in
        let copy = bootstrap_mapper.signature bootstrap_mapper in
        bootstrap copy pp signature
    | Partial_implementation _ -> error_annot "Partial_implementation"
    | Partial_interface _ -> error_annot "Partial_interface"
    | Packed _ -> error_annot "Packed"
  with
  | Cmt_format.Error (Not_a_typedtree _) ->
      Result.Error "Invalid cmt/cmti"

let kind filename =
  if not (Sys.file_exists filename) then `Ignore
  else if Sys.is_directory filename then `Dir
  else
    match Filename.extension filename with
    | ".cmt" -> `Cmt
    | ".cmti" -> `Cmti
    | _ -> `Ignore

let rec process stats filename =
  match kind filename with
  | `Ignore -> ()
  | `Dir ->
      let files = Sys.readdir filename in
      let filepaths = Array.map (Filename.concat filename) files in
      Array.iter (process stats) filepaths
  | `Cmt | `Cmti ->
      let test_name = Filename.basename filename |> Filename.remove_extension in
      print_string ("test '" ^ test_name ^ "' : ");
      begin match bootstrap_cmt filename with
      | Ok () ->
          print_string "pass";
          stats.pass <- stats.pass + 1
      | Error msg ->
          print_string ("error: " ^ msg);
          stats.fail <- stats.fail + 1
      end;
      print_newline ()

let () =
  let stats = { pass = 0; fail = 0 } in
  print_endline "Bootstrap Tests";
  print_endline "---------------";
  for i = 1 to Array.length Sys.argv - 1 do
    process stats Sys.argv.(i)
  done;
  print_newline ();
  print_stats stats;
  print_endline "---------------"
