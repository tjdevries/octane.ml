type ('a, 'b, 'multiplicity) t = ('a, 'b) Query.t * 'multiplicity

(* This trick can be used to hide the type... I think we can use this *)
(* I wonder if we can hide the type of the fields from the table, for example *)
(* type wrapped_ty_list = MkWrappedTyList : 'a Type.ty_list -> wrapped_ty_list *)

let make_one (query : ('a, 'b) Query.t) : ('a, 'b, [< `One ]) t =
  let _ = query in
  query, `One
;;
