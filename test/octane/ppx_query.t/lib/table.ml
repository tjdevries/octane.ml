module User = struct
  type t =
    { id : int [@primary_key { autoincrement = true }]
    ; name : string
    ; age : int
    }
  [@@deriving table { name = "users" }]
end

let%query (module UserNameQuery) = "select User.id, User.name from User"
