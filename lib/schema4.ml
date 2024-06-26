[@@@ocaml.warning "-7-11-32-33-39-27"]

type sql_string = [ `string ]
type sql_bytes = [ `bytes ]
type sql_blob = [ `blob ]
type sql_float = [ `float ]
type sql_int = [ `int ]
type sql_bool = [ `bool ]

type sql_type =
  [ sql_string
  | sql_bytes
  | sql_blob
  | sql_float
  | sql_int
  | sql_bool
  ]

type sql_numeric =
  [ sql_float
  | sql_int
  ]

type sql_textual =
  [ sql_string
  | sql_bytes
  | sql_blob
  ]

(* hey idiot, don't forget that you want to insert into the database. it's kind
   of a key feature of the whole database thing. like the data part. *)
module Column : sig
  type name

  type 'a t =
    | STRING : name -> sql_string t
    | INTEGER : name -> sql_int t

  val name : 'a t -> name
  val to_string : 'a t -> string
  val field_concat : name list -> string
  val mk_string : 'tbl -> string -> sql_string t
  val mk_int : 'tbl -> string -> sql_int t
end = struct
  type name = string

  type 'a t =
    | STRING : name -> sql_string t
    | INTEGER : name -> sql_int t

  let name : 'a. 'a t -> name =
    fun (type a) (x : a t) : name ->
    match x with
    | STRING x -> x
    | INTEGER x -> x
  ;;

  let to_string : 'a. 'a t -> string = fun (type a) (x : a t) : string -> name x
  let field_concat xs = String.concat ", " xs

  (* Make columns *)
  let mk_string _tbl x = STRING x
  let mk_int _tbl x = INTEGER x
end

module Expr = struct
  module Primitive = struct
    type 'a t =
      | String : string -> sql_string t
      | Integer : int -> sql_int t
      | Float : float -> sql_float t
      | Bool : bool -> sql_bool t

    let to_type : 'a. 'a t -> 'a =
      fun (type a) (t : a t) : a ->
      match t with
      | String _ -> `string
      | Integer _ -> `int
      | Float _ -> `float
      | Bool _ -> `bool
    ;;

    let to_string : 'a. 'a t -> string =
      fun (type a) (t : a t) : string ->
      match t with
      | String str ->
        (* TODO: Escape sql injection here *)
        Format.sprintf "%S" str
      | Integer int -> Format.sprintf "%d" int
      | Float float -> Format.sprintf "%f" float
      | Bool bool -> Format.sprintf "%b" bool
    ;;
  end

  (* I wonder if this should have two types...
     Primitive types then just always match both types,
     but functions may need to store the type so that I can do something
     with it later. Just a lot of annoyingness to do this. *)
  type 'a t =
    | Primitive : 'a Primitive.t -> 'a t
    | Column : 'a Column.t -> 'a t
    | Binary : ('left t * string * 'right t) -> 'a t
    | Unary : (string * _ t) -> 'a t
    | Postary : (string * _ t) -> 'a t
    | Fn : (string * _ t) -> 'a t
    | Tuple : ('a t * 'b t) -> ('a * 'b) t

  let equals (type a) (x : a t) (y : a t) : sql_bool t = Binary (x, "=", y)
  let not_equals (type a) (x : a t) (y : a t) : sql_bool t = Binary (x, "<>", y)
  let greater_than (type a) (x : a t) (y : a t) : sql_bool t = Binary (x, ">", y)
  let less_than (type a) (x : a t) (y : a t) : sql_bool t = Binary (x, "<", y)
  let not_null x : sql_bool t = Postary ("IS NOT NULL", x)
  let and_ x y : sql_bool t = Binary (x, "AND", y)
  let or_ x y : sql_bool t = Binary (x, "OR", y)

  (* Operators *)
  let ( = ) x y = equals x y
  let ( <> ) x y = not_equals x y
  let ( > ) x y = greater_than x y
  let ( < ) x y = less_than x y
  let ( && ) x y = and_ x y
  let ( || ) x y = or_ x y

  (* Shorthands *)
  let c x = Column x
  let s str = Primitive (String str)
  let i int = Primitive (Integer int)
  let f float = Primitive (Float float)
  (* let sum expr = Fn ("SUM", [ expr ]) *)
  (* let max expr = Fn ("MAX", [ expr ]) *)

  let length (expr : [< sql_textual ] t) : sql_int t = Fn ("LENGTH", expr)

  let power (base : [< sql_numeric ] t) (exponent : [< sql_numeric ] t)
    : [< sql_numeric ] t
    =
    Fn ("POWER", Tuple (base, exponent))
  ;;

  let to_type t =
    match t with
    | Primitive expr -> Primitive.to_type expr
    (* TODO: Don't even know if this is possible... *)
    | _ -> assert false
  ;;

  let rec to_string : 'a. 'a t -> string =
    fun (type a) (t : a t) : string ->
    match t with
    | Binary (left, operator, right) ->
      let left = to_string left in
      let right = to_string right in
      Format.sprintf "(%s) %s (%s)" left operator right
    | Unary (operator, right) ->
      Format.sprintf "%s %s" operator (to_string right)
    | Postary (operator, right) ->
      Format.sprintf "%s %s" (to_string right) operator
    | Primitive expr -> Primitive.to_string expr
    | Column x -> Column.to_string x
    | Fn (fn, expr) -> Format.sprintf "%s(%s)" fn (to_string expr)
    | Tuple (left, right) ->
      Format.sprintf "%s, %s" (to_string left) (to_string right)
  ;;
end

module ExprList = struct
  type 'a items =
    | [] : unit items
    | ( :: ) : ('a Expr.t * 'b items) -> ('a * 'b) items

  type t = COLUMNS : 'a items -> t
  (* TODO: HERES MY IDEA, PUT A NEW PACKED TYPED HERE *)
  (* | JOINED : ('a, 'tbl) column_list * ('b, 'tbl) column_list -> t *)

  let empty () = COLUMNS []

  let rec to_names t =
    let rec aux : 'a. 'a items -> string list -> string list =
      fun (type a) (t : a items) (acc : string list) : string list ->
      match t with
      | [] -> List.rev acc
      | x :: xs -> aux xs (Expr.to_string x :: acc)
    in
    match t with
    | COLUMNS t -> aux t []
  ;;

  let rec to_types : 'a. 'a items -> 'a =
    fun (type a) (t : a items) : a ->
    (* let rec aux : 'a 'b. ('a * 'b) items -> 'b list -> 'a * 'b = *)
    (*   fun (type a b) (t : (a * b) items) (acc : b) : a -> *)
    (*   match t with *)
    (*   | [] -> acc *)
    (*   | x :: xs -> aux xs (Expr.to_string x :: acc) *)
    (* in *)
    let rec aux : 'a. 'a items -> 'a =
      fun (type a) (t : a items) : a ->
      match t with
      | [] -> ()
      | x :: [] -> Expr.to_type x, ()
      | x :: xs -> Expr.to_type x, aux xs
    in
    aux t
  ;;
end

(* need to track our "selected" values *)
type 'a query =
  | FROM : string * 'a -> 'a query
  | JOIN : 'a query * 'b query * ('a * 'b -> sql_bool Expr.t) -> ('a * 'b) query
  | SELECT : 'a query * ('a -> _ ExprList.items) -> 'a query
  | WHERE : 'a query * ('a -> _ Expr.t) -> 'a query

let from table = FROM (table#table_name, table)
let join from joined expr = JOIN (from, joined, expr)
let select fields query = SELECT (query, fields)
let where fields query = WHERE (query, fields)

type request =
  { name : string
  ; fields : ExprList.t
  ; join : (string * string) list
  ; where : string
  }

let rec build_request_tables : 'a. 'a query -> 'a =
  fun (type a) (query : a query) : a ->
  match query with
  | FROM (_, table) -> table
  | JOIN (from, joined, _) ->
    build_request_tables from, build_request_tables joined
  | SELECT (from, _) -> build_request_tables from
  | WHERE (from, _) -> build_request_tables from
;;

let rec build_request_type : 'a. 'a query -> request =
  fun (type a) (query : a query) : request ->
  match query with
  | FROM (name, table) ->
    { name; fields = ExprList.empty (); join = []; where = "" }
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
    let fields = ExprList.COLUMNS fields in
    { from with fields }
  | WHERE (from, cond) ->
    let expr = cond (build_request_tables from) in
    let expr = Expr.to_string expr in
    let from = build_request_type from in
    { from with where = expr }
;;

let build_request : 'a. 'a query -> string =
  fun (type a) (query : a query) : string ->
  let request = build_request_type query in
  let fields =
    match request.fields with
    | ExprList.COLUMNS [] -> "*"
    | fields -> ExprList.to_names request.fields |> String.concat ", "
  in
  let join =
    match request.join with
    | [] -> ""
    | [ (join, condition) ] ->
      Format.sprintf "\n  INNER JOIN %s ON %s" join condition
    | _ -> assert false
  in
  let where =
    match request.where with
    | "" -> ""
    | where -> Format.sprintf "\n  WHERE %s" where
  in
  Format.sprintf "SELECT %s\n  FROM %s %s%s" fields request.name join where
;;

class user_table =
  object (self)
    method private table : [ `user ] = `user
    method table_name = "user"
    method id = Expr.c @@ Column.mk_int self#table "user.id"
    method name = Expr.c @@ Column.mk_string self#table "user.name"

    (* Auto generated fields *)
    method fields = ExprList.[ self#id; self#name ]
  end

let user = new user_table

(* THIS WILL BE GENERATED VIA PPX FROM YOUR RECORD (OR SIMILAR) *)
type post =
  { id : string
  ; user_id : int
  ; title : string
  ; views : int
  }

class post_table =
  object (self)
    method private table : [ `post ] = `post
    method table_name = "post"
    method id = Expr.c @@ Column.mk_string self#table "post.id"
    method user_id = Expr.c @@ Column.mk_int self#table "post.user_id"
    method title = Expr.c @@ Column.mk_string self#table "post.title"
    method views = Expr.c @@ Column.mk_int self#table "post.views"
  end

let post = new post_table

(* Some example queries *)
let spelled_out = from user |> select (fun user -> ExprList.[ user#id ])
let example = Expr.Tuple Expr.(s "hello", Tuple (s "wow", i 1))

let query =
  let open Expr in
  let from_user = from user in
  let from_post = from post in
  join from_user from_post (fun (user, post) -> user#id = post#user_id)
  |> where (fun (user, post) -> length post#title > i 10)
  |> select (fun (user, post) -> [ user#id; post#title; user#name ])
;;
