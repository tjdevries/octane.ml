open Base
open Riot

let ( let* ) = Stdlib.Result.bind

open Logger.Make (struct
    let namespace = [ "bin"; "oql_run" ]
  end)

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

let%query (module UserName) = "SELECT User.id, User.name FROM User"

let example db =
  let* users = UserName.query db in
  let users = Option.value users ~default:[] in
  List.iter users ~f:(fun { id; name } ->
    Fmt.pr "@.We read this from the database: %d - %s@." id name);
  Ok ()
;;

(* TODO: Should be an error *)
let%query (module GetPost) =
  "SELECT User.id, Post.author, Post.content FROM Post WHERE Post.id = $id"
;;

let _ = "Post.Author.name"

let _ =
  {| SELECT User.name, Post.content
      FROM Post INNER JOIN User ON User.id = Post.author
      WHERE Post.id = $id
    |}
;;

let get_post_example db =
  let* post = GetPost.query db ~id:1 in
  let post = Option.value post ~default:[] in
  List.iter post ~f:(fun { author; content } ->
    Fmt.pr "@.Post: %d - %s@." author content);
  Ok ()
;;

(* generated_query_one db ~id deserialize *)
let generated_query_one db ~id deserialize =
  let query = "" in
  Fmt.epr "query: %s@." query;
  Silo_postgres.query db ~params:[ Number id ] ~query ~deserializer:deserialize
;;

(* generated_query_two db ~id:(Number id) deserialize *)
let generated_query_two db ~id deserialize =
  let query = "" in
  Fmt.epr "query: %s@." query;
  Silo_postgres.query db ~params:[ id ] ~query ~deserializer:deserialize
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
      Silo_postgres.config
        ~connections:2
        ~connection_string:
          "postgresql://tjdevries:password@localhosting:5432/oql?sslmode=disable"
    in
    match Silo_postgres.connect ~config with
    | Ok c -> Ok c
    | Error (`Msg e) -> failwith ("connection:" ^ e)
    | _ -> failwith "connection: unknown error"
  in
  let users =
    match UserName.query db with
    | Ok one -> one
    | Error e -> failwith e
  in
  let _ =
    match users with
    | Some users ->
      List.iter
        ~f:(fun { id; name } -> Fmt.pr "@.This is from riot: %d - %s@." id name)
        users
    | _ -> Fmt.pr "@.Shouldn't be possible@."
  in
  Ok 1
;;
