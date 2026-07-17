type stats = { mutable pass : int; mutable fail : int }

let print_stats { pass; fail } =
  let total = pass + fail in
  print_endline ("Pass: " ^ string_of_int pass);
  print_endline ("Fail: " ^ string_of_int fail);
  print_endline ("Total: " ^ string_of_int total)

let bootstrap_cmt filename =
  let bootstrap of_ to_ pp original =
    let vaast = of_ original in
    let ocaml = to_ vaast in
    (* TODO: write comparison functions for OCaml.Typedtree (and
       Vaast.Typedtree). Relying on the "string"-representation of
       OCaml.Typedtree does not provide all the details and is only temporary.
    *)
    let str_of pp x =
      pp Format.str_formatter x;
      Format.flush_str_formatter ()
    in
    let expected = str_of pp original in
    let got = str_of pp ocaml in
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
        bootstrap VT.of_structure VT.to_structure pp structure
    | Interface signature ->
        let pp = Printtyped.interface in
        bootstrap VT.of_signature VT.to_signature pp signature
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
