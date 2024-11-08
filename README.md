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


# TODOs:
- Table & SQL syntax generation needs to be provided by "Driver" for different SQL dialects.
    - We have different stuff for SQlite vs Postgres


### Random Thoughts

```ocaml
module Constraints = struct
  (* This is how you can extend the generated constraints *)
  (* include Constraints *)
  (* let table = [
        PrimaryKey [ Fields.id ];
        Raw "ADD CONSTRAINT chk_users_status CHECK (status IN ('active', 'inactive', 'pending'));"
      ] *)
end

(* id must be passed, nothing special happens *)
type _primary_key = { id : int [@primary_key] }

(* id cannot be passed *)
type _with_autoincrement = { id : int [@primary_key { autoincrement = true }] }

(* it would be optional, but could be specified *)
type _with_default =
  { id : string [@primary_key { default = "uuid_generate_v1()" }] }

```
