module User = struct
  type t =
    { id : string
    ; name : string
    ; age : int
    }
  [@@deriving table { name = "users" }]
end

module Post = struct
  type t =
    { id : string
    ; user_id : string
    ; title : string
    }
  [@@deriving table { name = "posts" }]
end

(* Create a new module with a query for selecting name and age from User *)
let%query (module UserNamesAndAges) = "select User.name, User.age from User"

(* Variant Syntax *)
let%query WithoutModule = "select User.name, User.age from User"
