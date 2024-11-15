module User = struct
  type t =
    { id : int [@primary_key { autoincrement = true }]
    ; name : string
    ; middle_name : string option
    }

  and x = { id : int } [@@deriving table { name = "users" }]
end
