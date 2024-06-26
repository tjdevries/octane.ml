[@@@ocaml.warning "-7-11-32-33-39-27"]

class type table_user = object
  method user : unit
end

class type table_post = object
  method post : unit
end

let print_user : < user : unit ; .. > -> unit = fun user -> ()
let print_post : < post : unit ; .. > -> unit = fun user -> ()
let print_both : < user : unit ; post : unit > -> unit = fun user -> ()

(* example query: *)
(* select x.name, y.date from x join y on x.id = y.id *)

(* The tension is that i want to prevent using fields from other tables.
   But! I also need to support joining tables *)

(* val join :
   'a table ->
   'b table ->
   on:('a table * 'b table -> bool expr) ->
   ('a * 'b) query *)

let user =
  object
    method table : [ `user ] = `user
    method id : string = "id"
    method name : string = "name"
  end
;;

let post =
  object
    method table : [ `post ] = `post
    method id : string = "id"
    method title : string = "title"
  end
;;

let tabler (tbl : < table : 'a ; .. >) = tbl#table
let x = tabler user
let y = tabler post
