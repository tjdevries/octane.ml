type table =
  { name : string
  ; fields : string list
  }
[@@deriving show]

let registry : table list ref = ref []
let register table = registry := table :: !registry
