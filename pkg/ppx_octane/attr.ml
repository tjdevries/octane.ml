open Core
open Ppxlib

let make_deriving_attr ~loc names =
  (* Ast_helper.Exp.tuple *)
  (* List.map names ~f:(fun name -> *)
  (*   let ident = Loc.make ~loc (Lident name) in *)
  (*   let structure_item = Ast_helper.Str.eval (Ast_helper.Exp.ident ident) in *)
  (*   let payload = PStr [ structure_item ] in *)
  (*   Ast_helper.Attr.mk (Loc.make ~loc "deriving") payload) *)
  let exprs =
    List.map names ~f:(fun name ->
      let ident = Loc.make ~loc (Lident name) in
      Ast_helper.Exp.ident ident)
  in
  let tuple = Ast_helper.Exp.tuple exprs in
  let structure_item = Ast_helper.Str.eval tuple in
  let payload = PStr [ structure_item ] in
  Ast_helper.Attr.mk (Loc.make ~loc "deriving") payload
;;

let make_ignore_warning ~loc =
  let const = Ast_builder.Default.estring ~loc "-32" in
  let structure_item = Ast_helper.Str.eval const in
  let payload = PStr [ structure_item ] in
  Ast_helper.Attr.mk (Loc.make ~loc "warning") payload
;;
