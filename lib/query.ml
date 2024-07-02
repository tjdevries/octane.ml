module QueryTypes = struct
  type ('table, 'fields) select =
    { table : 'table Schema.table
    ; fields : 'fields Schema.field_list
    ; limit : int option
    }

  let pp_select fmt { table; fields; limit } =
    let limit =
      match limit with
      | None -> ""
      | Some limit -> Format.sprintf "\nLIMIT %d" limit
    in
    Format.fprintf
      fmt
      "SELECT %a\nFROM %s%s"
      Schema.pp_field_list
      fields
      table.name
      limit
  ;;
end

(* TODO: This is where we can add update, etc. *)
type ('a, 'b) t = SELECT of ('a, 'b) QueryTypes.select

(* How to assert that the fields have to be from the table? *)
(* Not sure if we even should... because what if you have a join,
   that sounds like a nightmare to validate*)
let select : table:'a Schema.table -> 'b Schema.field_list -> ('a, 'b) t =
  fun (type a b) ~(table : a Schema.table) (fields : b Schema.field_list) ->
  SELECT { table; fields; limit = None }
;;

let limit : limit:int -> ('a, 'b) t -> ('a, 'b) t =
  fun ~limit query ->
  match query with
  | SELECT select -> SELECT { select with limit = Some limit }
;;

let pp_query fmt query =
  match query with
  | SELECT select -> QueryTypes.pp_select fmt select
;;
