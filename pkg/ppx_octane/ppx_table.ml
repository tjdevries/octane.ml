open Core
open Ppxlib
open Ast_builder
module Database = Drivers.Sqlite

module FieldKind = struct
  type t =
    | PrimaryKey of { autoincrement : bool }
    | Column
  [@@deriving eq]
end

module TableField = struct
  type t =
    { label_declaration : label_declaration
    ; loc : Location.t
    ; name : string Loc.t
    ; ty : core_type
    ; kind : FieldKind.t
    }

  let make label_declaration =
    let kind =
      List.find_map label_declaration.pld_attributes ~f:(fun attr ->
        match attr.attr_name.txt with
        | "primary_key" -> Some (FieldKind.PrimaryKey { autoincrement = true })
        | _ -> None)
      |> Option.value ~default:FieldKind.Column
    in
    { label_declaration
    ; loc = label_declaration.pld_loc
    ; name = label_declaration.pld_name
    ; ty = label_declaration.pld_type
    ; kind
    }
  ;;

  (* Iter helpers *)
  let map (fields : t list) ~f = List.map ~f:(fun t -> f ~loc:t.loc t) fields

  (* SQL Helpers *)
  let create_field ~loc t =
    let txt_to_sql = function
      | "int" -> "INTEGER"
      | "string" -> "TEXT"
      | _ -> failwith "TODO: create_field - unknown type"
    in
    let rec coretype_to_create_field ty =
      match ty with
      | [%type: [%t? core_type] option] ->
        let _, field = coretype_to_create_field core_type in
        true, field
      | { ptyp_desc = Ptyp_constr ({ txt; _ }, []); _ } -> begin
        match txt with
        | Lident txt -> false, txt_to_sql txt
        | Ldot (Ldot (Lident m, "Fields"), f) ->
          (* let module_param = Gen.module_param ~loc m f in *)
          (* [%expr [%e module_param] [%e ename]] *)
          false, "INTEGER"
        | _ -> failwith "TODO: create_field - unknown type"
      end
      (* | _ -> failwith "TODO: create_field - unknown type" *)
      | _ -> Location.Error.raise (Location.Error.createf ~loc "Unknown type")
    in
    let nullable, column_type = coretype_to_create_field t.ty in
    let column_attributes =
      match nullable, t.kind with
      | _, FieldKind.PrimaryKey { autoincrement = true } ->
        "PRIMARY KEY AUTOINCREMENT"
      | false, FieldKind.PrimaryKey { autoincrement = false } ->
        "PRIMARY KEY NOT NULL"
      | false, FieldKind.Column -> "NOT NULL"
      | true, FieldKind.PrimaryKey _ -> "PRIMARY KEY"
      | true, FieldKind.Column -> ""
    in
    Format.sprintf "%s %s %s" t.name.txt column_type column_attributes
  ;;

  (* AST Helpers *)
  let ename { loc; name; _ } = Default.evar ~loc name.txt
end

let make_fields_from_type payload =
  let checker =
    object
      inherit [label_declaration list] Ast_traverse.fold as super

      method! label_declaration ext acc =
        super#label_declaration ext (ext :: acc)
    end
  in
  checker#type_declaration payload [] |> List.rev |> List.map ~f:TableField.make
;;

let args () = Deriving.Args.(empty +> arg "name" (estring __))

let get_field_constructor ~loc ename pld_type =
  let match_lident name optional =
    match name, optional with
    | "int", true -> [%expr DBCaml.Params.Values.integer_opt [%e ename]]
    | "int", false -> [%expr DBCaml.Params.Values.integer [%e ename]]
    | "string", true -> [%expr DBCaml.Params.Values.text_opt [%e ename]]
    | "string", false -> [%expr DBCaml.Params.Values.text [%e ename]]
    | lident, _ ->
      Fmt.failwith "TODO: field_params - unknown builtin type: %s" lident
  in
  let rec coretype_to_expr ty optional =
    match ty.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "option"; _ }, [ core_type ]) ->
      coretype_to_expr core_type true
    | Ptyp_constr ({ txt; _ }, []) -> begin
      match txt with
      | Lident ident -> match_lident ident optional
      | Ldot (Ldot (Lident m, "Fields"), f) ->
        let module_param = Gen.module_param ~loc m f in
        [%expr [%e module_param] [%e ename]]
      | Ldot _ -> failwith "TODO: unknown ldot"
      | Lapply (_, _) -> failwith "TODO: Lapply"
    end
    | _ -> failwith "TODO: field_params"
  in
  coretype_to_expr pld_type false
;;

let generate_impl ~ctxt (_rec_flag, type_declarations) (name : string option) =
  let type_declarations : type_declaration list = type_declarations in
  let ty = List.hd_exn type_declarations in
  let fields = make_fields_from_type ty in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let name =
    match name with
    | Some name -> name
    | None -> failwith "name is required"
  in
  let ename = Default.estring ~loc name in
  let field_names =
    List.map fields ~f:(fun { loc; name; _ } ->
      let pat = Default.ppat_var ~loc name in
      let str = Default.estring ~loc name.txt in
      [%stri let [%p pat] = [%e str]])
  in
  let field_types =
    List.map fields ~f:(fun { loc; name; label_declaration; _ } ->
      let attrs =
        [ Attr.make_deriving_attr ~loc [ "deserialize"; "serialize" ] ]
      in
      let type_decl =
        Ast_helper.Type.mk name ~manifest:label_declaration.pld_type ~attrs
      in
      Ast_helper.Str.type_ Recursive [ type_decl ])
  in
  let field_params =
    TableField.map fields ~f:(fun ~loc field ->
      let pat = Default.ppat_var ~loc field.name in
      let ename = TableField.ename field in
      let param_name = Default.ppat_var ~loc field.name in
      let constructor = get_field_constructor ~loc ename field.ty in
      [%stri let [%p param_name] = fun [%p pat] -> [%e constructor]])
  in
  let field_module = Ast_helper.Mod.structure (field_names @ field_types) in
  let params_module = Ast_helper.Mod.structure field_params in
  let insert =
    let fields =
      List.filter fields ~f:(fun field -> FieldKind.equal field.kind Column)
    in
    let params =
      TableField.map fields ~f:(fun ~loc field ->
        let ename = TableField.ename field in
        let param_ident =
          Loc.make ~loc (Ldot (Lident "Params", Loc.txt field.name))
        in
        let param = Default.pexp_ident ~loc param_ident in
        [%expr [%e param] [%e ename]])
      |> Default.elist ~loc
    in
    let columns =
      TableField.map fields ~f:(fun ~loc field -> field.name.txt)
      |> String.concat ~sep:", "
    in
    let placeholders =
      List.map fields ~f:(fun _ -> "?") |> String.concat ~sep:", "
    in
    let query =
      [%string
        "INSERT INTO %{name} (%{columns}) VALUES (%{placeholders}) RETURNING *"]
      |> Default.estring ~loc
    in
    let body =
      [%expr
        match
          DBCaml.query
            db
            ~params:[%e params]
            ~query:[%e query]
            ~deserializer:deserialize_row
        with
        | Ok [ t ] -> Ok t
        | Ok [] -> Error (`msg "empty: Should have returned one item")
        | Ok _ -> Error (`msg "empty: Should not return more than one item")
        | Error err -> Error err]
    in
    let body = Gen.make_positional_fun ~loc "db" body in
    let body =
      List.fold_right fields ~init:body ~f:(fun field acc ->
        if String.(field.name.txt = "middle_name")
        then Gen.make_optional_fun ~loc field.name.txt acc
        else Gen.make_labelled_fun ~loc field.name.txt acc)
    in
    [%stri let insert = [%e body]]
  in
  let deser =
    Serde_derive.De.generate_impl ~ctxt (_rec_flag, type_declarations)
  in
  let ser =
    Serde_derive.Ser.generate_impl ~ctxt (_rec_flag, type_declarations)
  in
  let drop_query = Default.estring ~loc (Database.drop_table ~name) in
  let create_query =
    let columns =
      TableField.map fields ~f:TableField.create_field
      |> String.concat ~sep:", "
    in
    Default.estring ~loc (Database.create_table ~name ~columns)
  in
  deser
  @ ser
  @ [ [%stri type row = t list [@@deriving serialize, deserialize]]
    ; [%stri let relation = [%e ename]]
    ; [%stri module Fields = [%m field_module]]
    ; [%stri module Params = [%m params_module]]
    ; insert
    ; [%stri
        module Table = struct
          let drop db = DBCaml.execute db ~params:[] ~query:[%e drop_query]
          let create db = DBCaml.execute db ~params:[] ~query:[%e create_query]
        end]
    ; [%stri
        let () = Octane.TableRegistry.register { name = "test"; fields = [] }]
    ]
;;

let generator () = Deriving.Generator.V2.make (args ()) generate_impl
let my_deriver = Deriving.add "table" ~str_type_decl:(generator ())
