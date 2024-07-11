open Core
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
    let ast =
      match Oql.Run.parse query with
      | Ok ast -> ast
      | Error msg -> failwith msg
    in
    (* Fmt.epr "@.=====@.query: %s@." query; *)
    (* Fmt.epr "%a@.======@." Oql.Ast.pp ast; *)
    let items =
      match ast with
      | Oql.Ast.Select { select; from = Some relation; where } ->
        let open Oql.Ast in
        let expressions = get_select_expressions select in
        let fields =
          List.filter_map
            ~f:(fun e ->
              match e with
              | Column col -> assert false
              | ModelField model_field ->
                let open Ast_builder.Default in
                let m = ModelField.model_name model_field in
                let f_start, f_end, f = model_field.field in
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
        (* let _ = Ast_builder.Default.ptyp_extension *)
        let invalid_model = Oql.Analysis.get_invalid_model ast in
        let items =
          match invalid_model with
          | None ->
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
            let attributes =
              Attr.make_deriving_attr ~loc [ "serialize"; "deserialize" ]
            in
            let type_decl =
              { type_decl with ptype_attributes = [ attributes ] }
            in
            let type_decl =
              Ast_builder.Default.pstr_type ~loc Recursive [ type_decl ]
            in
            let query_fn = Gen.of_ast ~loc ast in
            (* [%p arg1] *)
            (* let arg1 = Ast_builder.Default.ppat_var ~loc (Loc.make ~loc "db") in *)
            [ type_decl
            ; [%stri
                module Query = struct
                  type query = t list [@@deriving deserialize, serialize]
                end]
            ; [%stri let deserialize = Query.deserialize_query]
            ; query_fn
            ]
          | Some invalid_model ->
            (* let loc = Oql.Ast.Model.location invalid_model in *)
            let loc =
              add_location
                query_loc
                (Oql.Ast.ModelField.location invalid_model)
                2
            in
            (* let _ = Ast_builder.Default.plb in *)
            let ty =
              Ast_builder.Default.ptyp_extension ~loc
              @@ Location.error_extensionf
                   ~loc
                   "Invalid Model: Module '%a' is not selected in query"
                   Oql.Ast.Model.pp
                   invalid_model.model
            in
            [ [%stri type t = [%t ty]] ]
        in
        items
      | _ -> []
    in
    let query = Ast_builder.Default.estring ~loc query in
    let ignore_warning = Attr.make_ignore_warning ~loc in
    let x =
      { pmod_desc = Pmod_structure (items @ [ [%stri let raw = [%e query]] ])
      ; pmod_loc = loc
      ; pmod_attributes = [ ignore_warning ]
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

Driver.register_transformation
  ~rules:[ extender_rule; letter_rule ]
  "ppx_octane"
