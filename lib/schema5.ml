open Schema4

type 'a from =
  | FROM : string * 'a -> 'a from
  | JOIN : 'a from * 'b from * ('a * 'b -> sql_bool Expr.t) -> ('a * 'b) from

type ('a, 'b) query =
  { from : 'a from
  ; where : ('a -> sql_bool Expr.t) option
  ; select : ('a -> 'b ExprList.items) option
  }

(* type ('a, 'b, 'c) request = 'a from * ('a * 'b) query * ('a * 'c) query *)
type ('a, 'b, +'m) request =
  { (* id: int option; *)
    (* query: Caqti_driver_info.t -> query; *)
    (* param_type: 'a Caqti_type.t; *)
    (* row_type: 'b Caqti_type.t; *)
    (* row_mult: 'm Caqti_mult.t; *)
    id : int option
  ; query : ('a, 'b) query
  ; row_mult : 'm
  }
  constraint 'm = [< `Zero | `One | `Many ]

let join x y f = JOIN (x, y, f)
let select ?where select from = { from; where; select = Some select }

module From = struct
  let get_from_name : 'a. 'a from -> string =
    fun (type a) (from : a from) : string ->
    match from with
    | FROM (name, _) -> name
    | JOIN (FROM (name, _), _, _) -> name
    | _ -> failwith "get_from_name: missing from base case"
  ;;

  let rec get_tables_from : 'a. 'a from -> 'a =
    fun (type a) (from : a from) : a ->
    match from with
    | FROM (_, table) -> table
    | JOIN (x, y, _) -> get_tables_from x, get_tables_from y
  ;;

  let joins : 'a. 'a from -> (string * string) option =
    fun (type a) (from : a from) : (string * string) option ->
    match from with
    | FROM _ -> None
    | JOIN (x, y, expr) ->
      let y_name = get_from_name y in
      let x, y = get_tables_from x, get_tables_from y in
      let expr = expr (x, y) in
      let expr = Expr.to_string expr in
      Some (y_name, expr)
  ;;
end

let print_query query =
  let from_tables = From.get_tables_from query.from in
  let selection =
    match query.select with
    | None -> "*"
    | Some select ->
      let selection = select from_tables in
      ExprList.to_names (COLUMNS selection) |> String.concat ", "
  in
  let join =
    match From.joins query.from with
    | None -> ""
    | Some (table, condition) ->
      Format.sprintf "\n  INNER JOIN %s ON %s" table condition
  in
  let where =
    match query.where with
    | None -> ""
    | Some where ->
      let where = where from_tables in
      Format.sprintf "\n  WHERE %s" (Expr.to_string where)
  in
  let from_table = From.get_from_name query.from in
  Format.sprintf "SELECT %s\n  FROM %s %s%s" selection from_table join where
;;

module Types = struct
  type t =
    | Integer of int
    | String of string
    | Float of float
    | Bool of bool
    | Null
    | Array of t list
end

module type SQL_DECODER = sig
  val of_database : sql_type -> string -> Types.t
end

module DecodeInteger = struct
  let of_database _ str = Types.Integer (int_of_string str)
end

module DecodeString = struct
  let of_database _ str = Types.String str
end

(* Not sure if this is a good idea... need some way to decode *)
let get_decoder (sql_type : sql_type) : (module SQL_DECODER) =
  match sql_type with
  | `int -> (module DecodeInteger)
  | `string -> (module DecodeString)
  | _ -> assert false
;;

let rec get_decoder_from_expr : 'a. 'a -> (module SQL_DECODER) =
  fun sql_type -> assert false
;;

(* Hmmm... this doesn't feel right. I think I'm doing something wrong here. *)
(* Maybe I should be matching directly on the expresion type instead. *)
let get_decoder_from_query : 'a. (_, 'a) query -> (module SQL_DECODER) =
  fun (type a) (query : (_, a) query) : (module SQL_DECODER) ->
  match query.select with
  | None -> assert false
  | Some select ->
    let selection = select (From.get_tables_from query.from) in
    let types = ExprList.to_types selection in
    let _decoder = get_decoder_from_expr types in
    assert false
;;

let get_decoder_from_expr : 'a. 'a Expr.t -> (module SQL_DECODER) =
  fun (type a) (expr : a Expr.t) : (module SQL_DECODER) ->
  (*  Ok... this would work for single values....
      but how do i do this for multiple values?
      Still have to figure out how to unpack this into a record for example *)
  match expr with
  | Primitive (Integer _) -> (module DecodeInteger)
  | Primitive (String _) -> (module DecodeString)
  | _ -> assert false
;;

(*  Hmmm... i wonder if i have to implement this with macros :'( *)
(* Was really hoping I could avoid here, but don't know how else to make this really easy *)
(* Could at least generate the simple ones for retrieving a whole table *)
let on_row (v1, (v2, (v3, ()))) = v1, v2, v3

let query =
  let open Expr in
  let user_table = FROM (user#table_name, user) in
  let post_table = FROM (post#table_name, post) in
  join user_table post_table (fun (user, post) -> user#id = post#user_id)
  |> select
       (fun (user, post) -> user#fields)
       ~where:(fun (user, post) -> user#id = i 1 && not_null user#id)
;;

let decoder = get_decoder `int

let should_be_int =
  let module Decoder = (val decoder : SQL_DECODER) in
  let y = Decoder.of_database `int "1" in
  match y with
  | Types.Integer x -> x
  | _ -> assert false
;;

let () = Format.printf "SHOULD BE INT: %d@." should_be_int

(* let execute : 'a. _ -> (_, 'a) query -> 'a = assert false *)
(* let map db query = *)
(*   let result = execute db query in *)
(*   result *)
(* ;; *)
(* let result = execute () query *)
