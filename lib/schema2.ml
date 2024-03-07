type ('table, 'field) field =
  { name : string
  ; field_type : 'field Type.t
  ; table : 'table
  ; primary_key : bool
  }

type 'a table =
  { name : string
  ; table : 'a
  }

let user_table : [ `user ] table = { name = "user"; table = `user }
let post_table : [ `post ] table = { name = "post"; table = `post }
let tags_table : [ `tag ] table = { name = "tag"; table = `tag }
let print_table prefix table = Format.printf "%s: %s@." prefix table.name

let print_table_table table =
  match table.table with
  | `user -> print_table "USER->" table
  | `post -> print_table "POST->" table
;;

let _ = print_table_table user_table
let _ = print_table_table post_table

(* Fails, not allowed *)
(* let _ = print_table_table tags_table *)

let this_only_works_with_user_table table =
  match table.table with
  | `user -> true
;;

let _ = this_only_works_with_user_table user_table
(* Both fail, not `user *)
(* let _ = this_only_works_with_user_table post_table *)
(* let _ = this_only_works_with_user_table tags_table *)

let field_user_id =
  { name = "user_id"
  ; field_type = Type.INTEGER
  ; table = `user
  ; primary_key = true
  }
;;

let field_user_name =
  { name = "user_name"
  ; field_type = Type.TEXT
  ; table = `user
  ; primary_key = true
  }
;;

let field_is_with_table : 'a. 'a table -> ('a, _) field -> unit =
  fun (type a) (table : a table) (field : (a, _) field) -> ()
;;

(* First one should pass *)
let y = field_is_with_table user_table field_user_id

(* This one should fail *)
(* let x = field_is_with_table post_table field_user_id *)

let fields_in_either_table : 'a table -> 'b table -> (_, _) field -> unit =
  fun t1 t2 field -> ()
;;

let z = fields_in_either_table user_table post_table field_user_id
