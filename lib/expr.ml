module Field = struct
  type 'a t =
    | STRING : string -> string t
    | INTEGER : int t

  let s f = STRING f
end

module Expr = struct
  type 'a t =
    | STRING : string -> string t
    | INTEGER : int -> int t
    | FIELD : 'a Field.t -> 'a t
    | EQ : 'a t * 'a t -> bool t

  let ( = ) l r = EQ (l, r)
  let s e = STRING e
  let field e = FIELD e
end

type ('relation, 'expr) query =
  | Select of
      { content : string
      ; relation : 'relation
      ; where : 'expr Expr.t
      }

(* let exec db ~id : t list = *)
(*   let query = *)
(*     Format.sprintf *)
(*       "select * from %s where %s = $1" *)
(*       User.relation *)
(*       User.Fields.id *)
(*   in *)
(*   Database.exec db ~query ~params:[ String id ] ~deserializer *)
(* ;; *)

(* module User = Tables.User *)
(**)
(* (* let exec = [%exec select * from User where User.id = $1] *) *)
(* let exec _db ~id = *)
(*   let _ = *)
(*     Select *)
(*       { content = *)
(*           Format.sprintf *)
(*             "SELECT * FROM %s WHERE %s = $1" *)
(*             User.relation *)
(*             User.Fields.id *)
(*       ; relation = User.relation *)
(*       ; where = Expr.(field (Field.s User.Fields.id) = s id) *)
(*       } *)
(*   in *)
(*   assert false *)
(* ;; *)
