module User = struct
  type t = { id : int } [@@deriving table { name = "users" }]
end

let%query (module UserByID) =
  "SELECT User.id, User.name FROM User WHERE User.id = $id"
;;
