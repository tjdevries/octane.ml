open Base
open Ppxlib
open Ast_builder

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

let generate_impl ~ctxt (_rec_flag, type_declarations) (name : string option) =
  let type_declarations : type_declaration list = type_declarations in
  let ty = List.hd_exn type_declarations in
  let names = f ty in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let name =
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
      let type_decl = Ast_helper.Type.mk pld_name ~manifest:pld_type in
      Ast_helper.Str.type_ Recursive [ type_decl ])
  in
  let field_module = Ast_helper.Mod.structure (field_names @ field_types) in
  [ [%stri let relation = [%e name]]
  ; [%stri module Fields = [%m field_module]]
  ]
;;

let generator () = Deriving.Generator.V2.make (args ()) generate_impl
let my_deriver = Deriving.add "table" ~str_type_decl:(generator ())
