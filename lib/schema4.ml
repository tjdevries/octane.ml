module Column : sig
  type 'a t
  type name

  val name : 'a t -> name
  val to_string : 'a t -> string
  val field_concat : name list -> string
  val mk_string : string -> string t
  val mk_int : string -> int t
end = struct
  type name = string

  type 'a t =
    | STRING : name -> string t
    | INTEGER : name -> int t

  let name : 'a. 'a t -> name =
    fun (type a) (x : a t) : name ->
    match x with
    | STRING x -> x
    | INTEGER x -> x
  ;;

  let to_string : 'a. 'a t -> string = fun (type a) (x : a t) : string -> name x
  let field_concat xs = String.concat ", " xs

  (* Make columns *)
  let mk_string x = STRING x
  let mk_int x = INTEGER x
end

module ColumnList = struct
  type 'a t =
    | [] : unit t
    | ( :: ) : ('a Column.t * 'b t) -> ('a * 'b) t
end

let user =
  object
    method table : [ `user ] = `user
    method table_name = "user"
    method id = Column.mk_int "user.id"
    method name = Column.mk_string "user.name"
  end
;;

let post =
  object
    method table : [ `post ] = `post
    method table_name = "post"
    method id = Column.mk_string "post.id"
    method user_id = Column.mk_string "post.user_id"
    method title = Column.mk_string "post.title"
    method views = Column.mk_int "post.views"
  end
;;

module Value = struct
  type 'a t =
    | FIELD : 'a Column.t -> 'a t
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

type 'a query =
  | FROM : string * 'a -> 'a query
  | JOIN : 'a query * 'b query * ('a * 'b -> 'expr Expr.t) -> ('a * 'b) query
  | SELECT : 'a query * ('a -> Column.name list) -> 'a query

let from table = FROM (table#table_name, table)
let join from joined expr = JOIN (from, joined, expr)
let from_user = from user
let from_post = from post
let selected = SELECT (from_user, fun x -> [ Column.name x#id ])

(* let joined = *)
(*   let open Expr in *)
(*   (* join from_user from_post [%sql user#id = post#user_id] *) *)
(*   join from_user from_post (fun (user, post) -> c user#id = c post#user_id) *)
(* ;; *)

let joined =
  let open Expr in
  join from_user from_post (fun (user, post) -> c user#id = i 5)
;;

(* let joined = *)
(*   let open Expr in *)
(*   join from_user from_post (fun (user, _) -> c user#id <> s "teej-dv") *)
(* ;; *)

let selected_joined =
  SELECT
    ( joined
    , fun (user, post) -> [ post#title; user#name ] |> List.map Column.name )
;;

type request =
  { name : string
  ; fields : Column.name list
  ; join : (string * string) list
  }

let rec build_request_tables : 'a. 'a query -> 'a =
  fun (type a) (query : a query) : a ->
  match query with
  | FROM (_, table) -> table
  | JOIN (from, joined, _) ->
    build_request_tables from, build_request_tables joined
  | _ -> assert false
;;

let rec build_request_type : 'a. 'a query -> request =
  fun (type a) (query : a query) : request ->
  match query with
  | FROM (name, table) -> { name; fields = ColumnList.[]; join = [] }
  | JOIN (from, joined, cond) ->
    let expr = cond (build_request_tables from, build_request_tables joined) in
    let expr = Expr.to_string expr in
    let from = build_request_type from in
    let join = build_request_type joined in
    { from with join = [ join.name, expr ] }
  | SELECT (from, f) ->
    let tables = build_request_tables from in
    let fields = f tables in
    let from = build_request_type from in
    { name = from.name; fields; join = from.join }
;;

let build_request : 'a. 'a query -> string =
  fun (type a) (query : a query) : string ->
  let request = build_request_type query in
  let fields =
    match request.fields with
    | [] -> "*"
    | fields -> Column.field_concat fields
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