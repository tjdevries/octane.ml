module User = struct
  type t =
    { id : int
    ; name : string
    ; age : int
    }
  [@@deriving table { name = "users" }]
end

let%query (module UserWithID) =
  "select User.id, User.name from User where User.id = $id"
;;
