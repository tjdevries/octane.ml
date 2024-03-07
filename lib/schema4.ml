module Column : sig
  type ('a, 'tbl) t
  type name

  val name : ('a, 'tbl) t -> name
  val to_string : ('a, 'tbl) t -> string
  val field_concat : name list -> string
  val mk_string : 'tbl -> string -> (string, 'tbl) t
  val mk_int : 'tbl -> string -> (int, 'tbl) t
end = struct
  type name = string

  type ('a, 'tbl) t =
    | STRING : 'tbl * name -> (string, 'tbl) t
    | INTEGER : 'tbl * name -> (int, 'tbl) t

  let name : 'a 'b. ('a, 'b) t -> name =
    fun (type a b) (x : (a, b) t) : name ->
    match x with
    | STRING (_, x) -> x
    | INTEGER (_, x) -> x
  ;;

  let to_string : 'a 'b. ('a, 'b) t -> string =
    fun (type a b) (x : (a, b) t) : string -> name x
  ;;

  let field_concat xs = String.concat ", " xs

  (* Make columns *)
  let mk_string tbl x = STRING (tbl, x)
  let mk_int tbl x = INTEGER (tbl, x)
end

module Value = struct
  type 'a t =
    | FIELD : ('a, 'b) Column.t -> 'a t
    | STRING : string -> string t
    | INTEGER : int -> int t

  let to_string : 'a. 'a t -> string =
    fun (type a) (t : a t) : string ->
    match t with
    | FIELD x -> Column.to_string x
    | STRING x -> Format.sprintf "%S" x
    | INTEGER x -> Format.sprintf "%d" x
  ;;
end

module Expr = struct
  type 'a t =
    | Equal of ('a Value.t * 'a Value.t)
    | NotEqual of ('a Value.t * 'a Value.t)

  let ( = ) x y = Equal (x, y)
  let ( <> ) x y = NotEqual (x, y)

  (* c for column *)
  (* let col x = Value.ColumnName x *)
  (* let column x = Value.ColumnName x *)
  let c x = Value.FIELD x
  let s str = Value.STRING str
  let i int = Value.INTEGER int

  let to_string t =
    match t with
    | Equal (left, right) ->
      Format.sprintf "%s = %s" (Value.to_string left) (Value.to_string right)
    | NotEqual (left, right) ->
      Format.sprintf "%s <> %s" (Value.to_string left) (Value.to_string right)
  ;;
end

module ColumnList = struct
  type ('a, 'b) column_list =
    | [] : (unit, 'b) column_list
    | ( :: ) :
        (('a, 'tbl) Column.t * ('b, 'tbl) column_list)
        -> ('a * 'b, 'tbl) column_list

  type t = COLUMNS : ('a, 'tbl) column_list -> t

  let empty () = COLUMNS []

  let rec to_names t =
    let rec aux : 'a 'tbl. ('a, 'tbl) column_list -> string list -> string list =
      fun (type a tbl)
        (t : (a, tbl) column_list)
        (acc : string list)
        : string list ->
      match t with
      | [] -> List.rev acc
      | x :: xs -> aux xs (Column.to_string x :: acc)
    in
    match t with
    | COLUMNS t -> aux t []
  ;;
end

type ('a, 'tbl) query =
  | FROM : string * 'a * 'tbl -> ('a, 'tbl) query
  | JOIN :
      ('a, 't1) query * ('b, 't2) query * ('a * 'b -> 'expr Expr.t)
      -> ('a * 'b, 't1 * 't2) query
  | SELECT :
      ('a, 'tbl) query * ('a -> (_, 'tbl) ColumnList.column_list)
      -> ('a, 'tbl) query

let from table = FROM (table#table_name, table, table#table)
let join from joined expr = JOIN (from, joined, expr)
let select fields query = SELECT (query, fields)

type request =
  { name : string
  ; fields : ColumnList.t
  ; join : (string * string) list
  }

let rec build_request_tables : 'a 'b. ('a, 'b) query -> 'a =
  fun (type a b) (query : (a, b) query) : a ->
  match query with
  | FROM (_, table, _) -> table
  | JOIN (from, joined, _) ->
    build_request_tables from, build_request_tables joined
  | _ -> assert false
;;

let rec build_request_type : 'a 'b. ('a, 'b) query -> request =
  fun (type a b) (query : (a, b) query) : request ->
  match query with
  | FROM (name, table, _) -> { name; fields = ColumnList.empty (); join = [] }
  | JOIN (from, joined, cond) ->
    let expr = cond (build_request_tables from, build_request_tables joined) in
    let expr = Expr.to_string expr in
    let from = build_request_type from in
    let join = build_request_type joined in
    { from with join = [ join.name, expr ] }
  | SELECT (from, f) ->
    let tables = build_request_tables from in
    let from = build_request_type from in
    let fields = f tables in
    let fields = ColumnList.COLUMNS fields in
    { name = from.name; fields; join = from.join }
;;

let build_request : 'a 'b. ('a, 'b) query -> string =
  fun (type a b) (query : (a, b) query) : string ->
  let request = build_request_type query in
  let fields =
    match request.fields with
    | ColumnList.COLUMNS [] -> "*"
    | fields -> ColumnList.to_names request.fields |> String.concat ", "
  in
  let join =
    match request.join with
    | [] -> ""
    | [ (join, condition) ] ->
      Format.sprintf "\n  INNER JOIN %s ON %s" join condition
    | _ -> assert false
  in
  Format.sprintf "SELECT %s\n  FROM %s %s" fields request.name join
;;

let user =
  object (self)
    (* method table = `user *)
    method table : [ `user ] = `user
    method table_name = "user"
    method id = Column.mk_int self#table "user.id"
    method name = Column.mk_string self#table "user.name"
  end
;;

let post =
  object (self)
    method table : [ `post ] = `post
    method table_name = "post"
    method id = Column.mk_string self#table "post.id"
    method user_id = Column.mk_int self#table "post.user_id"
    method title = Column.mk_string self#table "post.title"
    method views = Column.mk_int self#table "post.views"
  end
;;

let example x =
  match x#table with
  | `post -> "post"
  | `user -> "user"
;;

(* let select_id = select (fun tbl -> ColumnList.[ tbl#id; tbl#name; post#id ]) *)
(* let select_name = select (fun tbl -> ColumnList.[ tbl#name ]) *)

(* SELECT user.name
   FROM user *)
(* let user_query = from user |> select_id *)

(* let user_name_query = from user |> select_name *)
let spelled_out = from user |> select (fun user -> ColumnList.[ user#id ])
(* let post_name_query = from post |> select_name *)

(* SELECT user.name, post.title, user.id
   FROM user
   INNER JOIN post ON user.id = 7 *)
let query =
  let open Expr in
  let from_user = from user in
  let from_post = from post in
  let joined = join from_user from_post (fun (user, post) -> c user#id = i 7) in
  select (fun (user, post) -> [ user#id; post#title; user#name ]) joined
;;
