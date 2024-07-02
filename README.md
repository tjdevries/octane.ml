# `octane.ml`

> The fastest, the hottest

## Usage

Create a User model. The model has two columns. 

> **Note:** Migrations coming later

```ocaml
module User = struct
  type t =
    { id : int
    ; name : string
    }
  [@@deriving table { name = "users" }]
end
```

After you have a model, you can write typesafe queries!

```ocaml
let%query (module UserName) = "SELECT User.id, User.name FROM User"

let print_users db =
  let* users = UserName.query db in
  List.iter users ~f:(fun { id; name } ->
    Fmt.pr "@.We read this from the database: %d - %s@." id name);
  Ok ()
;;
```

There's more things too, but I haven't written those parts yet.
