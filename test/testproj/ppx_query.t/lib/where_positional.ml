module User = struct
  type t =
    { id : int
    ; name : string
    }
  [@@deriving table { name = "users" }]
end

let%query (module UserByID) =
  "SELECT User.name, $2 FROM User WHERE User.id = $1"
;;
