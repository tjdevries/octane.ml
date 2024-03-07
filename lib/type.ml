(* TODO: CUSTOM types *)

type 'a t =
  | NULLABLE : 'a t -> 'a option t
  | INTEGER : int t
  | REAL : float t
  | TEXT : string t
  | BOOLEAN : bool t

let null_ty : 'a. 'a t -> 'a option t =
  fun (type a) (ty : a t) : a option t ->
  match ty with
  | INTEGER -> NULLABLE INTEGER
  | REAL -> NULLABLE REAL
  | TEXT -> NULLABLE TEXT
  | BOOLEAN -> NULLABLE BOOLEAN
  | NULLABLE _ -> invalid_arg "already a nullable type"
;;

(* let pp (type a) (ty : a t) : Format.formatter -> a -> unit = *)
(*   fun fmt x -> *)
(*     match ty with *)
(*     | INTEGER -> Format.pp_print_int fmt x *)
(*     | REAL -> Format.pp_print_int fmt x *)

(* type 'a field = table_name * string * 'a Type.t *)

(* type 'a expr_list = *)
(*   | [] : unit expr_list *)
(*   | (::) : ('a expr * 'b expr_list) -> ('a * 'b) expr_list *)
