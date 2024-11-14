open Core
open Riot

let ( let* ) = Stdlib.Result.bind

open Logger.Make (struct
    let namespace = [ "bin"; "oql_run" ]
  end)

module User = struct
  type t =
    { id : int [@primary_key { autoincrement = true }]
    ; name : string
    ; middle_name : string option
    }
  [@@deriving table { name = "users" }]
end

module Post = struct
  type t =
    { id : int [@primary_key { autoincrement = true }]
    ; user_id : int [@references User.id { on_delete = `cascade }]
    ; content : string
    }
  [@@deriving table { name = "posts" }]
end

(* Use the generated functions to control the database *)
let setup_tables db =
  (* Create User Tables *)
  let* _ = User.Table.drop db in
  let* _ = User.Table.create db in
  (* Create Post Tables *)
  let* _ = Post.Table.drop db in
  let* _ = Post.Table.create db in
  Ok ()
;;

(* Insert some records into the database *)
let insert_examples db =
  let* _ = User.insert db ~name:"ThePrimeagen" ~middle_name:"KEKW" in
  let* user = User.insert db ~name:"teej_dv" ~middle_name:"lua" in
  Fmt.pr "  User: id:%d, name:%s@." user.id user.name;
  let* post = Post.insert ~user_id:user.id ~content:"Hello" db in
  Fmt.pr "  Post: id:%d, user: %d@." post.id post.user_id;
  Ok ()
;;

(* Select all users with their name and middle name *)
let%query (module UserName) = "SELECT User.id, User.name, User.middle_name FROM User"

let user_table_example db =
  let* users = UserName.query db in
  List.iter
    ~f:(fun { id; name; middle_name } ->
      let middle_name = Option.value middle_name ~default:"<missing>" in
      Fmt.pr "  UserName: id:%d, name:%s (%s)@." id name middle_name)
    users;
  Ok ()
;;

(* Select the user's name and all their posts *)
let%query (module GetPost) =
  {| SELECT User.name, Post.user_id, Post.content
      FROM Post
        INNER JOIN User ON User.id = Post.user_id
        WHERE User.id = $user_id |}
;;

let post_table_example db ~user_id =
  let* posts = GetPost.query db ~user_id in
  List.iter posts ~f:(fun { name; content; _ } ->
    Fmt.pr "  GetPost : author:%s, content:%s@." name content);
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
  set_log_level (Some Warn);
  info (fun f -> f "Starting application");
  let config =
    DBCaml.config
      ~connector:(module DBCamlSqlite.Connector)
      ~connections:1
      ~connection_string:"./sqlite/test.db"
  in
  let* db = DBCaml.connect ~config in
  info (fun f -> f "Finished connecting");
  let* _ = setup_tables db in
  Fmt.pr "==== CREATE ====@.";
  let* _ = insert_examples db in
  Fmt.pr "@.==== READ ====@.";
  let* _ = user_table_example db in
  let* _ = post_table_example db ~user_id:2 in
  Ok 1
;;
