open Schema4
open Schema5

(* So I guess the problem is that the Caqti_type is private... so I can't do this?
   I will have to ask ocaml experts later about this one. Was hoping I could do this to
   test it out. *)
(* let expr_to_caqti : 'b. _ Expr.t -> _ Caqti_type.t = *)
(*   fun (type b) (expr : _ Expr.t) : _ Caqti_type.t -> *)
let rec expr_to_caqti expr : 'a Caqti_type.t =
  match expr with
  | Expr.Primitive (Integer _) -> Caqti_type.field Int
  (* Ya, cannot figure out why this doesn't let me do this... *)
  (* | Expr.Primitive (String _) -> Caqti_type.field String *)
  | Expr.Column (Column.INTEGER _) -> Caqti_type.field Int
  | _ -> assert false
;;

let rec expr_items_to_caqti expr_list : 'a Caqti_type.t =
  match expr_list with
  | [] -> assert false
  | [ expr ] -> expr_to_caqti expr
  | _ -> assert false
;;

let example_query =
  let open Expr in
  let user_table = FROM (user#table_name, user) in
  select (fun user -> [ user#id ]) user_table
;;

let caqti_thing =
  match example_query.select with
  | None -> assert false
  | Some select ->
    let selection = select (From.get_tables_from example_query.from) in
    let types = selection in
    (* Hmm... I don't know how to write this generically *)
    let [ something ] = types in
    expr_to_caqti something
;;
