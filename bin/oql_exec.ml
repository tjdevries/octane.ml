open Core
open Riot

(* module Constraints = struct *)
(*   (* This is how you can extend the generated constraints *) *)
(*   (* include Constraints *) *)
(*   (* let table = [ *)
  (*       PrimaryKey [ Fields.id ]; *)
  (*       Raw "ADD CONSTRAINT chk_users_status CHECK (status IN ('active', 'inactive', 'pending'));" *)
  (*     ] *) *)
(* end *)

let ( let* ) = Stdlib.Result.bind

open Logger.Make (struct
    let namespace = [ "bin"; "oql_run" ]
  end)

(* primary key, autoincrement, default, not null *)

(* let example db = User.insert ~name:"foo" ~phone_number:"123" db *)
(* let example db = User.insert ~id:1 ~name:"foo" ~phone_number:"123" db *)
(* let example db = User.insert' { name = "foo"; phone_number = "123" } db *)

(* id must be passed, nothing special happens *)
type _primary_key = { id : int [@primary_key] }

(* id cannot be passed *)
type _with_autoincrement = { id : int [@primary_key { autoincrement = true }] }

(* it would be optional, but could be specified *)
type _with_default =
  { id : string [@primary_key { default = "uuid_generate_v1()" }] }

module User = struct
  type t =
    { id : int [@primary_key { autoincrement = true }]
    ; name : string
    ; phone_number : string
    ; middle_name : string option
    }
  [@@deriving table { name = "users" }]
end

module Post = struct
  type t =
    { id : int [@primary_key { autoincrement = true }]
    ; author : User.Fields.id
    ; content : string
    }
  [@@deriving table { name = "posts" }]
end

let%query (module UserName) =
  "SELECT User.id, User.name, User.middle_name FROM User"
;;

let%query (module GetPost) =
  {| SELECT User.name, Post.author, Post.content
      FROM Post
        INNER JOIN User ON User.id = Post.author
        WHERE User.id = $user_id |}
;;

let get_post_example db =
  let* _ = Post.Table.drop db in
  let* _ = Post.Table.create db in
  let* post = Post.insert ~author:1 ~content:"Hello" db in
  Fmt.pr "Inserted post: %d@." post.id;
  let* posts = GetPost.query db ~user_id:1 in
  List.iter posts ~f:(fun { name; content; _ } ->
    Fmt.pr "Post: %s - %s@." name content);
  Ok ()
;;

let () =
  Riot.run_with_status ~on_error:(fun x -> failwith (DBCaml.Error.show x))
  @@ fun () ->
  let _ =
    match Logger.start () with
    | Error (`Msg e) -> failwith e
    | Error `Supervisor_error -> failwith "SUPERVISOR"
    | Error (`Application_error msg) -> failwith msg
    | Ok pid -> pid
  in
  set_log_level (Some Logger.Info);
  (* set_log_level (Some Logger.Trace); *)
  info (fun f -> f "Starting application");
  let* db =
    let config =
      DBCaml.config
        ~connector:(module DBCamlSqlite.Connector)
        ~connections:1
        ~connection_string:"./sqlite/test.db"
    in
    match DBCaml.connect ~config with
    | Ok c -> Ok c
    | Error _ -> failwith "NO CONNECT"
  in
  info (fun f -> f "Finished connecting");
  let* _ = User.Table.drop db in
  let* _ = User.Table.create db in
  let* user =
    User.insert db ~name:"teej_dv" ~phone_number:"1234567" ~middle_name:"hi"
    (* ?middle_name:(Some "hi") *)
  in
  info (fun f -> f "Retrieved: %d - %s" user.id user.name);
  let* users = UserName.query db in
  List.iter
    ~f:(fun { id; name; middle_name } ->
      Fmt.pr
        "This is from riot: %d - %s | %s@."
        id
        name
        (Option.value middle_name ~default:"<missing>"))
    users;
  let* _ = get_post_example db in
  Ok 1
;;
