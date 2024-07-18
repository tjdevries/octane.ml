module User = struct
  type t =
    { id : int
    ; name : string
    ; age : int
    }
  [@@deriving table { name = "users" }]
end

let%query (module UserNameQuery) = "select User.id, User.name from User"
