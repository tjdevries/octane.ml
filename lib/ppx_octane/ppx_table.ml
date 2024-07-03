open Base
open Ppxlib
open Ast_builder

let print_stuff name () =
  match Bos.OS.Dir.current (), name with
  | Ok dir, Some name ->
    Fmt.pr "current dir: %s // %s@." (Fpath.to_string dir) name
  | _ -> Fmt.pr "cannot get current dir@."
;;

let f payload =
  let checker =
    object
      inherit [label_declaration list] Ast_traverse.fold as super

      method! label_declaration ext acc =
        super#label_declaration ext (ext :: acc)
    end
  in
  checker#type_declaration payload [] |> List.rev
;;

let args () = Deriving.Args.(empty +> arg "name" (estring __))

let get_field_constructor ~loc ename pld_type =
  let match_lident = function
    | "int" -> [%expr Dbcaml.Params.Number [%e ename]]
    | "string" -> [%expr Dbcaml.Params.String [%e ename]]
    | _ -> failwith "TODO: field_params - unknown builtin type"
  in
  match pld_type.ptyp_desc with
  | Ptyp_constr ({ txt; _ }, []) -> begin
    match txt with
    | Lident ident -> match_lident ident
    | Ldot (Ldot (Lident m, "Fields"), f) ->
      let module_param = Gen.module_param ~loc m f in
      [%expr [%e module_param] [%e ename]]
    | Ldot _ -> failwith "TODO: unknown ldot"
    | Lapply (_, _) -> failwith "TODO: Lapply"
  end
  | _ -> failwith "TODO: field_params"
;;

let generate_impl ~ctxt (_rec_flag, type_declarations) (name : string option) =
  let type_declarations : type_declaration list = type_declarations in
  let ty = List.hd_exn type_declarations in
  let names = f ty in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let ename =
    match name with
    | Some name -> Default.estring ~loc name
    | None -> failwith "name is required"
  in
  let field_names =
    List.map names ~f:(fun { pld_name; pld_type; pld_loc; _ } ->
      let name = Default.ppat_var ~loc:pld_loc pld_name in
      let str = Default.estring ~loc:pld_loc (Loc.txt pld_name) in
      [%stri let [%p name] = [%e str]])
  in
  let field_types =
    List.map names ~f:(fun { pld_name; pld_type; pld_loc; _ } ->
      let attrs =
        [ Attr.make_deriving_attr ~loc:pld_loc [ "deserialize"; "serialize" ] ]
      in
      let type_decl = Ast_helper.Type.mk pld_name ~manifest:pld_type ~attrs in
      Ast_helper.Str.type_ Recursive [ type_decl ])
  in
  let field_params =
    List.map names ~f:(fun { pld_name; pld_type; pld_loc; _ } ->
      let name = Default.ppat_var ~loc:pld_loc pld_name in
      let ename = Default.evar ~loc:pld_loc (Loc.txt pld_name) in
      let param_name = Default.ppat_var ~loc:pld_loc pld_name in
      let constructor = get_field_constructor ~loc ename pld_type in
      [%stri let [%p param_name] = fun [%p name] -> [%e constructor]])
  in
  let field_module = Ast_helper.Mod.structure (field_names @ field_types) in
  let params_module = Ast_helper.Mod.structure field_params in
  let deser =
    Serde_derive.De.generate_impl ~ctxt (_rec_flag, type_declarations)
  in
  let ser =
    Serde_derive.Ser.generate_impl ~ctxt (_rec_flag, type_declarations)
  in
  if false then print_stuff name ();
  deser
  @ ser
  @ [ [%stri let relation = [%e ename]]
    ; [%stri module Fields = [%m field_module]]
    ; [%stri module Params = [%m params_module]]
    ; [%stri
        let () = Octane.TableRegistry.register { name = "test"; fields = [] }]
    ]
;;

let generator () = Deriving.Generator.V2.make (args ()) generate_impl
let my_deriver = Deriving.add "table" ~str_type_decl:(generator ())
