module OptionalField = struct
  type t =
    { id : int [@primary_key { autoincrement = true }]
    ; optional : string option
    }
  [@@deriving table { name = "optional_field" }]
end
