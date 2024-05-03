(* This is all copy pasta'd from caqti *)
(* I need to do some studying !! *)

(* this is pretty gigachad ocaml my caml here... *)
(* I can steal this idea for the types I think? *)
(* This way I can have a proper type? instead of polymorphic variants for everything. *)

type +'m t =
  (* not GADT due to variance *)
  | Zero
  | One
  | Zero_or_one
  | Zero_or_more
  constraint 'm = [< `Zero | `One | `Many ]

type zero = [ `Zero ]
type one = [ `One ]

type zero_or_one =
  [ `Zero
  | `One
  ]

type zero_or_more =
  [ `Zero
  | `One
  | `Many
  ]

let zero : [> `Zero ] t = Zero
let one : [> `One ] t = One
let zero_or_one : [> `Zero | `One ] t = Zero_or_one
let zero_or_more : ([> `Zero | `One | `Many ] as 'a) t = Zero_or_more

let only_zero : [< `Zero ] t -> unit = function
  | Zero -> ()
  | _ -> assert false
;;

let only_one : [< `One ] t -> unit = function
  | One -> ()
  | _ -> assert false
;;

let only_zero_or_one : [< `Zero | `One ] t -> unit = function
  | Zero | One -> ()
  | _ -> assert false
;;

let expose = function
  | Zero -> `Zero
  | One -> `One
  | Zero_or_one -> `Zero_or_one
  | Zero_or_more -> `Zero_or_more
;;

let can_be_zero = function
  | One -> false
  | Zero | Zero_or_one | Zero_or_more -> true
;;

let can_be_many = function
  | Zero | One | Zero_or_one -> false
  | Zero_or_more -> true
;;
