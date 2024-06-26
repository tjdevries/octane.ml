open Base
open Ppxlib
open Ast_builder

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
