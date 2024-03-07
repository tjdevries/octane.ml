type 'a field =
  { name : string
  ; field_type : 'a Type.t
  ; primary_key : bool
  }

type 'a field_list =
  | [] : unit field_list
  | ( :: ) : ('a field * 'b field_list) -> ('a * 'b) field_list

type wrapped_fields = PACKED : 'a field_list -> wrapped_fields

type !'a table =
  { name : string
  ; table : 'a
  ; fields : wrapped_fields
  }

let create_table name table fields = { name; table; fields = PACKED fields }

let column ?(primary_key = false) name field_type =
  { name; field_type; primary_key }
;;

let int = Type.INTEGER
let text = Type.TEXT

(* A bit goofy to unpack it this way... but it lets us hide the type for now *)
let null = Type.null_ty

(* let pp_field_list fmt ls = *)
(*   Format.pp_print_list *)
(*     ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") *)
(*     Format.pp_print_string *)
(*     fmt *)
(*     ls *)
(* ;; *)

let pp_field fmt (field : 'a field) = Format.fprintf fmt "%s" field.name

(* Printing field names, used in select for example *)
let rec pp_field_list_inner : 'a. Format.formatter -> 'a field_list -> unit =
  fun fmt (type a) (ls : a field_list) ->
  match ls with
  | [] -> ()
  | h :: t -> Format.fprintf fmt ", %a%a" pp_field h pp_field_list_inner t

and pp_field_list : 'a. Format.formatter -> 'a field_list -> unit =
  fun fmt (type a) (fields : a field_list) ->
  match fields with
  | [] -> ()
  | h :: t -> Format.fprintf fmt "%a%a" pp_field h pp_field_list_inner t
;;
