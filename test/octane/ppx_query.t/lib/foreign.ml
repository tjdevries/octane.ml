module User = struct
  type t =
    { id : int
    ; name : string
    }
  [@@deriving table { name = "users" }]
end

module Post = struct
  type t =
    { id : int
    ; author : User.Fields.id
    ; content : string
    }
  [@@deriving table { name = "posts" }]
end
