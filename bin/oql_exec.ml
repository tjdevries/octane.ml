open Core
open Riot

let ( let* ) = Stdlib.Result.bind

open Logger.Make (struct
    let namespace = [ "bin"; "oql_run" ]
  end)

module User = struct
  type t =
    { id : int [@primary_key]
    ; name : string
    ; phone_number : string
    }
  [@@deriving table { name = "users" }]

  module Constraints = struct
    (* This is how you can extend the generated constraints *)
    (* include Constraints *)
    (* let table = [
        PrimaryKey [ Fields.id ];
        Raw "ADD CONSTRAINT chk_users_status CHECK (status IN ('active', 'inactive', 'pending'));"
      ] *)
  
  end
end

module Post = struct
  type t =
    { id : int
    ; author : User.Fields.id
    ; content : string
    }
  [@@deriving table { name = "posts" }]
end

let%query (module UserName) = "SELECT User.id, User.name FROM User"

let _example db =
  let* users = UserName.query db in
  List.iter users ~f:(fun { id; name } ->
    Fmt.pr "@.We read this from the database: %d - %s@." id name);
  Ok ()
;;

let%query (module GetPost) =
  {| SELECT User.name, Post.author, Post.content
      FROM Post
        INNER JOIN User ON User.id = Post.author
        WHERE User.id = $user_id |}
;;

let get_post_example db =
  let* post = GetPost.query db ~user_id:1 in
  List.iter post ~f:(fun { name; content; _ } ->
    Fmt.pr "Post: %s - %s@." name content);
  Ok ()
;;

(* generated_query_one db ~id deserialize *)
let _generated_query_one db ~id deserialize =
  let query = "" in
  Fmt.epr "query: %s@." query;
  Silo.query db ~params:[ Number id ] ~query ~deserializer:deserialize
;;

(* generated_query_two db ~id:(Number id) deserialize *)
let _generated_query_two db ~id deserialize =
  let query = "" in
  Fmt.epr "query: %s@." query;
  Silo.query db ~params:[ id ] ~query ~deserializer:deserialize
;;

let () =
  Riot.run_with_status ~workers:2 ~on_error:(fun x -> failwith x)
  @@ fun () ->
  let _ =
    match Logger.start () with
    | Error (`Msg e) -> failwith e
    | Error `Supervisor_error -> failwith "SUPERVISOR"
    | Error (`Application_error msg) -> failwith msg
    | Ok pid -> pid
  in
  (* set_log_level (Some Logger.Trace); *)
  info (fun f -> f "Starting application");
  let* db =
    let config =
      Silo.config
        ~connections:2
        ~driver:(module Dbcaml_driver_postgres)
        ~connection_string:
          "postgresql://tjdevries:password@localhosting:5432/oql?sslmode=disable"
    in
    match Silo.connect ~config with
    | Ok c -> Ok c
    | Error e -> failwith ("connection:" ^ e)
  in
  let users =
    match UserName.query db with
    | Ok one -> one
    | Error e -> failwith e
  in
  List.iter
    ~f:(fun { id; name } -> Fmt.pr "This is from riot: %d - %s@." id name)
    users;
  let* _ = get_post_example db in
  Ok 1
;;
