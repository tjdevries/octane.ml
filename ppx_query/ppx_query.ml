open Ppxlib

let extender_rule =
  let context = Extension.Context.module_expr in
  let extender_name = "query" in
  let extracter () = Ast_pattern.(single_expr_payload (estring __)) in
  let expander ~ctxt s =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    (* [%stri module X = struct end] *)
    { pmod_desc = Pmod_structure [ [%stri type t = int] ]
    ; pmod_loc = loc
    ; pmod_attributes = []
    }
  in
  let my_extender =
    Extension.V3.declare extender_name context (extracter ()) expander
  in
  Context_free.Rule.extension my_extender
;;

let shift_start start offset colshift =
  Lexing.
    { pos_fname = start.pos_fname
    ; pos_lnum = start.pos_lnum + offset.pos_lnum - 1
    ; pos_cnum = start.pos_cnum + offset.pos_cnum + colshift
    ; pos_bol = start.pos_bol + offset.pos_bol
    }
;;

let shift_end start offset colshift =
  Lexing.
    { pos_fname = start.pos_fname
    ; pos_lnum = start.pos_lnum + offset.pos_lnum - 1
    ; pos_cnum = start.pos_cnum + offset.pos_cnum + colshift
    ; pos_bol = start.pos_bol + offset.pos_bol
    }
;;

let add_location start offset colshift =
  Location.
    { loc_start = shift_start start.loc_start offset.loc_start colshift
    ; loc_end = shift_end start.loc_start offset.loc_end colshift
    ; loc_ghost = false
    }
;;

let letter_rule =
  let context = Extension.Context.structure_item in
  let extender_name = "query" in
  let extracter () =
    let open Ast_pattern in
    (* let%query module [pattern] *)
    let qmod = ppat_unpack (some __) in
    (* let%query [pattern] *)
    let qident = ppat_construct (lident __) drop in
    (* THIS IS THE STRING I WANT TO FIND THE LOCATION OF *)
    let binding = value_binding ~pat:(alt qmod qident) ~expr:(estring __') in
    pstr (pstr_value nonrecursive (binding ^:: nil) ^:: nil)
    (* |> map_value' ~f:(fun loc (pat, query) -> loc, pat, query) *)
  in
  let my_extender =
    Extension.V3.declare extender_name context (extracter ())
    @@ fun ~ctxt pat query ->
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let query_loc = Loc.loc query in
    let query = Loc.txt query in
    (* Ast_traverse.fold *)
    (* let str_loc = Expansion_context.Extension.with_loc_and_path in *)
    Fmt.epr "@.=====@.query: %s@." query;
    let ast = Oql.Run.parse query in
    let ast =
      match ast with
      | Ok ast -> ast
      | Error msg -> failwith msg
    in
    Fmt.epr "%a@.======@." Oql.Ast.pp ast;
    let items =
      match ast with
      | Oql.Ast.Select { expressions; relation = Some "User" } ->
        let open Oql.Ast in
        let fields =
          List.filter_map
            (fun e ->
              match e with
              | ColumnReference
                  (Table (Module m), Field (f_start, f_end, Unquoted f)) ->
                let open Ast_builder.Default in
                let f_loc =
                  Location.
                    { loc_start = f_start; loc_end = f_end; loc_ghost = false }
                in
                let f_loc = add_location query_loc f_loc 1 in
                let ident = Ldot (Lident m, "Fields") in
                let ident = Ldot (ident, f) in
                let type_ : core_type =
                  ptyp_constr ~loc (Loc.make ~loc:f_loc ident) []
                in
                Some
                  (label_declaration
                     ~loc
                     ~name:(Loc.make ~loc f)
                     ~mutable_:Immutable
                     ~type_)
              | _ -> None)
            expressions
        in
        let type_decl =
          Ast_builder.Default.type_declaration
            ~loc
            ~name:(Loc.make ~loc "t")
            ~params:[]
            ~cstrs:[]
            ~kind:(Ptype_record fields)
            ~private_:Public
            ~manifest:None
        in
        let type_decl =
          Ast_builder.Default.pstr_type ~loc Recursive [ type_decl ]
        in
        [ type_decl
        ; [%stri let deserialize _ = assert false]
        ; [%stri
            let exec db =
              let query =
                Format.sprintf "select %s from %s" "users.name" "users"
              in
              let _ = db in
              (* Database.exec db ~query ~params:[] ~deserializer:deserialize *)
              query
            ;;]
        ]
      | _ -> []
    in
    let query = Ast_builder.Default.estring ~loc query in
    let x =
      { pmod_desc = Pmod_structure (items @ [ [%stri let raw = [%e query]] ])
      ; pmod_loc = loc
      ; pmod_attributes = []
      }
    in
    let binding =
      Ast_builder.Default.module_binding
        ~loc
        ~name:(Loc.make ~loc (Some pat))
        ~expr:x
    in
    { pstr_desc = Pstr_module binding; pstr_loc = loc }
  in
  Context_free.Rule.extension my_extender
;;

Driver.register_transformation ~rules:[ extender_rule; letter_rule ] "ppx_query"
